import { createReadStream, openSync } from 'node:fs';
import { mkdir, stat } from 'node:fs/promises';
import { homedir } from 'node:os';
import { basename, extname, resolve } from 'node:path';
import { createInterface } from 'node:readline/promises';

function readStdin() {
  return new Promise((resolveInput, reject) => {
    let input = '';

    process.stdin.setEncoding('utf8');
    process.stdin.on('data', (chunk) => {
      input += chunk;
    });
    process.stdin.on('end', () => {
      resolveInput(input);
    });
    process.stdin.on('error', reject);
  });
}

function stripJsonComments(input) {
  let output = '';
  let inString = false;
  let escaped = false;
  let inLineComment = false;
  let inBlockComment = false;

  for (let index = 0; index < input.length; index += 1) {
    const char = input[index];
    const next = input[index + 1];

    if (inLineComment) {
      if (char === '\n' || char === '\r') {
        inLineComment = false;
        output += char;
      }
      continue;
    }

    if (inBlockComment) {
      if (char === '*' && next === '/') {
        inBlockComment = false;
        index += 1;
      }
      continue;
    }

    if (inString) {
      output += char;

      if (escaped) {
        escaped = false;
      } else if (char === '\\') {
        escaped = true;
      } else if (char === '"') {
        inString = false;
      }

      continue;
    }

    if (char === '"') {
      inString = true;
      output += char;
      continue;
    }

    if (char === '/' && next === '/') {
      inLineComment = true;
      index += 1;
      continue;
    }

    if (char === '/' && next === '*') {
      inBlockComment = true;
      index += 1;
      continue;
    }

    output += char;
  }

  return output;
}

function parseProfile(input) {
  const jsonStart = input.indexOf('{');
  const jsonEnd = input.lastIndexOf('}');

  if (jsonStart === -1 || jsonEnd === -1 || jsonEnd < jsonStart) {
    throw new Error('No JSON object found in stdin.');
  }

  return JSON.parse(stripJsonComments(input.slice(jsonStart, jsonEnd + 1)));
}

function asStringArray(value) {
  if (!Array.isArray(value)) {
    return [];
  }

  return value.filter((entry) => typeof entry === 'string');
}

function expandPath(profilePath) {
  if (profilePath === '~') {
    return homedir();
  }

  if (profilePath.startsWith('~/')) {
    return resolve(homedir(), profilePath.slice(2));
  }

  return resolve(profilePath);
}

function containsGlob(profilePath) {
  return /[*?[\]{}]/.test(profilePath);
}

function isLikelyFile(profilePath) {
  const name = basename(profilePath);

  if (name === '' || name === '.' || name === '..') {
    return false;
  }

  if (extname(name) !== '') {
    return true;
  }

  return new Set([
    '.bash_profile',
    '.bashrc',
    '.gitconfig',
    '.gitignore',
    '.profile',
    '.zprofile',
    '.zshrc',
    'allowed_signers',
    'authorized_keys',
    'config',
    'known_hosts',
  ]).has(name);
}

async function shouldCreateDirectory(profilePath, expandedPath) {
  if (containsGlob(profilePath) || isLikelyFile(profilePath)) {
    return false;
  }

  try {
    await stat(expandedPath);
    return false;
  } catch (error) {
    if (error?.code !== 'ENOENT') {
      throw error;
    }

    return true;
  }
}

async function collectDirectoryPaths(profile) {
  const filesystem = profile?.filesystem ?? {};
  const profilePaths = [
    ...asStringArray(filesystem.allowRead),
    ...asStringArray(filesystem.allowWrite),
  ];
  const directoryPaths = new Set();

  for (const profilePath of profilePaths) {
    const expandedPath = expandPath(profilePath);

    if (await shouldCreateDirectory(profilePath, expandedPath)) {
      directoryPaths.add(expandedPath);
    }
  }

  return [...directoryPaths].sort((left, right) => left.localeCompare(right));
}

async function confirmCreation() {
  let terminalFd;

  try {
    terminalFd = openSync('/dev/tty', 'r');
  } catch {
    throw new Error('No interactive terminal available for confirmation.');
  }

  const readline = createInterface({
    input: createReadStream(null, { fd: terminalFd, autoClose: true }),
    output: process.stdout,
  });

  try {
    const answer = await readline.question('Create these directories? [y/N] ');
    return answer.trim().toLowerCase() === 'y';
  } finally {
    readline.close();
  }
}

async function main() {
  const input = await readStdin();
  const profile = parseProfile(input);
  const directoryPaths = await collectDirectoryPaths(profile);

  if (directoryPaths.length === 0) {
    console.log('No missing directories found in filesystem.allowRead or filesystem.allowWrite.');
    return;
  }

  console.log('Directories to create:');
  for (const directoryPath of directoryPaths) {
    console.log(directoryPath);
  }

  if (!(await confirmCreation())) {
    console.log('Aborted.');
    return;
  }

  for (const directoryPath of directoryPaths) {
    await mkdir(directoryPath, { recursive: true });
  }

  console.log(`Created ${directoryPaths.length} directories.`);
}

main().catch((error) => {
  console.error(error instanceof Error ? error.message : error);
  process.exit(1);
});
