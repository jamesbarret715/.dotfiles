-- 
-- wezterm - customisable terminal emulator
--

local wez = require 'wezterm'

-- EVENTS
wez.on('format-window-title', function()
    return 'wezterm'
end)

-- CONFIG
return {
    -- FONTS
    font = wez.font_with_fallback {
        'JetBrains Mono Nerd Font',
        'Phosphor'
    },

    font_size = 11.0,
    harfbuzz_features = { 'calt=1', 'clig=1', 'liga=1' },

    -- APPEARANCE
    enable_tab_bar = false,

    window_padding = {
        left = '10px',
        right = '10px',
        top = '10px',
        bottom = '10px',
    },

    window_background_opacity = 0.85
}

