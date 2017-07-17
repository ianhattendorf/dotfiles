local utils = require 'mp.utils'

local function exec(process)
    p_ret = utils.subprocess({args = process})
    if p_ret.error and p_ret.error == "init" then
        print("ERROR executable not found: " .. process[1])
    end
    return p_ret
end

-- check if BAT directories exist (BAT0, BAT1, etc.)
local battery_exists_exec = exec({"/bin/sh", "-c", "ls /sys/class/power_supply/BAT*"})

function is_desktop()
    return battery_exists_exec.status ~= 0
end

function is_laptop()
    return battery_exists_exec.status == 0
end

function on_battery()
    -- check if battery status is discharging
    -- assumes battery location
    local bat = exec({"/bin/sh", "-c", "/usr/bin/upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep discharging"})
    return bat.status == 0
end
