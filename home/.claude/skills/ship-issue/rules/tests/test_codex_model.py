import contextlib
import importlib.util
import io
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location('codex_model', Path(__file__).parents[1] / 'codex-model.py')
model = importlib.util.module_from_spec(spec)
spec.loader.exec_module(model)


class ModelSelectionTests(unittest.TestCase):
    def select(self, entries):
        with tempfile.TemporaryDirectory() as directory:
            cache = Path(directory) / 'catalog.json'
            cache.write_text(json.dumps({'models': entries}))
            with contextlib.redirect_stderr(io.StringIO()):
                return model.select_model(cache)

    def test_selects_astra(self):
        self.assertEqual(self.select([{'slug': 'gpt-6-astra'}]), 'gpt-6-astra')

    def test_new_model_does_not_replace_astra(self):
        self.assertEqual(self.select([{'slug': 'gpt-6-astra'}, {'slug': 'new-model'}]), 'gpt-6-astra')

    def test_upgrade_hint_does_not_change_pin(self):
        self.assertEqual(self.select([{'slug': 'gpt-6-astra', 'upgrade': {'model': 'new-model'}}]), 'gpt-6-astra')

    def test_missing_astra_has_no_fallback(self):
        self.assertIsNone(self.select([{'slug': 'gpt-6-sol'}]))

    def test_main_uses_configured_codex_home(self):
        with tempfile.TemporaryDirectory() as directory:
            (Path(directory) / 'models_cache.json').write_text(json.dumps({'models': []}))
            with patch.dict('os.environ', {'CODEX_HOME': directory}):
                with contextlib.redirect_stderr(io.StringIO()):
                    self.assertEqual(model.main(), 2)

    def test_missing_cache_attempts_explicit_pin(self):
        with tempfile.TemporaryDirectory() as directory:
            with contextlib.redirect_stderr(io.StringIO()) as errors:
                result = model.select_model(Path(directory) / 'absent')
            self.assertEqual(result, 'gpt-6-astra')
            self.assertIn('unchecked', errors.getvalue())


if __name__ == '__main__':
    unittest.main()
