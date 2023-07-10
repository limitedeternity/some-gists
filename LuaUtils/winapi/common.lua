local ffi = require("ffi")
local lk32 = require("waffi.windows.kernel32")

ffi.cdef [[
    size_t wcslen(const wchar_t *str);
]]

local ffi_defines = {
    INVALID_HANDLE_VALUE = ffi.cast("HANDLE", -1),
}

local winapi_common = {}

---@param str string?
---@return ffi.cdata*?, number
function winapi_common.utf8_to_wide_char(str)
    if not str then
        return nil, 0
    end

    local ptr, asize = ffi.cast("const char*", str), #str
    local nsize = lk32.MultiByteToWideChar(lk32.CP_UTF8, 0, ptr, asize, nil, 0)
    local wstr = ffi.new("wchar_t[?]", nsize + 1)

    nsize = lk32.MultiByteToWideChar(lk32.CP_UTF8, 0, ptr, asize, wstr, nsize)
    wstr[nsize] = 0

    return wstr, nsize
end

---@param wstr ffi.cdata*?
---@param size number?
---@return string?
function winapi_common.utf8_from_wide_char(wstr, size)
    if not wstr then
        return nil
    end

    if not size then
        size = ffi.C.wcslen(wstr)
    end

    local nsize = lk32.WideCharToMultiByte(lk32.CP_UTF8, 0, wstr, size, nil, 0, nil, nil)
    local astr = ffi.new("char[?]", nsize + 1)

    nsize = lk32.WideCharToMultiByte(lk32.CP_UTF8, 0, wstr, size, astr, nsize, nil, nil)
    astr[nsize] = 0

    return ffi.string(astr)
end

---@param handle ffi.cdata*
---@return nil
function winapi_common.close_handle(handle)
    if handle ~= ffi.NULL and handle ~= ffi_defines.INVALID_HANDLE_VALUE then
        lk32.CloseHandle(handle)
    end
end

return winapi_common
