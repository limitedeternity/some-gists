local ffi = require("ffi")
local lk32 = require("waffi.windows.kernel32")
local winver = require("waffi.windows.version")
local winapi_common = require("winapi.common")

ffi.cdef [[
    typedef struct tagVS_FIXEDFILEINFO
    {
        DWORD   dwSignature;            /* e.g. = 0xfeef04bd */
        DWORD   dwStrucVersion;         /* e.g. = 0x00000042 = "0.42" */
        DWORD   dwFileVersionMS;        /* e.g. = 0x00030075 = "3.75" */
        DWORD   dwFileVersionLS;        /* e.g. = 0x00000031 = "0.31" */
        DWORD   dwProductVersionMS;     /* e.g. = 0x00030010 = "3.10" */
        DWORD   dwProductVersionLS;     /* e.g. = 0x00000031 = "0.31" */
        DWORD   dwFileFlagsMask;        /* = = 0x3F for version "0.42" */
        DWORD   dwFileFlags;            /* e.g. VFF_DEBUG | VFF_PRERELEASE */
        DWORD   dwFileOS;               /* e.g. VOS_DOS_WINDOWS16 */
        DWORD   dwFileType;             /* e.g. VFT_DRIVER */
        DWORD   dwFileSubtype;          /* e.g. VFT2_DRV_KEYBOARD */
        DWORD   dwFileDateMS;           /* e.g. 0 */
        DWORD   dwFileDateLS;           /* e.g. 0 */
    }
    VS_FIXEDFILEINFO;

    static const DWORD VS_FFI_SIGNATURE = 0xFEEF04BD;

    typedef struct _TRANSLATION
    {
        union
        {
            WORD array[2];
            struct
            {
                WORD language;
                WORD codepage;
            };
        };
    }
    TRANSLATION;
]]

local MtComposer = require("mtcomposer")

---@class Verinfo
local Verinfo = {}

---@param path string
---@return table?
function Verinfo:new(path)
    local wchar_path, _ = winapi_common.utf8_to_wide_char(path)
    if not wchar_path then
        return nil
    end

    local verinfo_size = winver.GetFileVersionInfoSizeW(wchar_path, nil)
    if verinfo_size == 0 then
        return nil
    end

    local verinfo_buf = ffi.new("BYTE[?]", verinfo_size)
    if winver.GetFileVersionInfoW(wchar_path, 0, verinfo_size, verinfo_buf) == 0 then
        return nil
    end

    ---@param key_path string
    ---@param type ffi.cdecl* | ffi.ctype*
    ---@return ffi.cdata*?, number
    local function query_verinfo(key_path, type)
        local wchar_key, _ = winapi_common.utf8_to_wide_char(key_path)
        if not wchar_key then
            return nil, 0
        end

        local value_ptr = ffi.new("LPVOID[1]")
        local value_size = ffi.new("ULONG[1]")

        if winver.VerQueryValueW(verinfo_buf, wchar_key, value_ptr, value_size) == 0 or value_size[0] == 0 then
            return nil, 0
        end

        return ffi.cast(type, value_ptr[0]), value_size[0]
    end

    local impl = {}

    ---@return ffi.cdata*?
    function impl:query_root()
        local root, _ = query_verinfo([[\]], "VS_FIXEDFILEINFO*")
        if not root then
            return nil
        end

        if root[0].dwSignature ~= ffi.C.VS_FFI_SIGNATURE then
            return nil
        end

        return root[0]
    end

    ---@param param string
    ---@return string?
    function impl:query_fileinfo(param)
        local locale, _ = query_verinfo([[\VarFileInfo\Translation]], "TRANSLATION*")
        local language, codepage =
            locale and locale[0].language or lk32.GetUserDefaultLangID(),
            locale and locale[0].codepage or lk32.GetACP()

        local key_path = string.format([[\StringFileInfo\%04x%04x\%s]], language, codepage, param)
        local value_ptr, value_size = query_verinfo(key_path, "const wchar_t*")

        if not value_ptr then
            return nil
        end

        return winapi_common.utf8_from_wide_char(value_ptr, value_size)
    end

    setmetatable(impl,
        MtComposer
        :init()
        .Class(self, impl)
        :done()
    )

    return impl
end

setmetatable(Verinfo,
    MtComposer
    :init()
    .Callable(Verinfo.new)
    :done()
)

return Verinfo
