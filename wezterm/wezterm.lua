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

    font_size = 11,
    harfbuzz_features = { 'calt=1', 'clig=1', 'liga=1' },

    -- APPEARANCE
    enable_tab_bar = false,

    window_padding = {
        left = '12px',
        right = '12px',
        top = '12px',
        bottom = '12px',
    },

    window_background_opacity = 0.9
}

