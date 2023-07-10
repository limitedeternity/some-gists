local MtComposer = require("mtcomposer")

function MtComposer.Bitable(rtbl)
    return {
        __newindex = function (tbl, key, value)
            if value == nil then
                return
            end

            if not rtbl[value] then
                rtbl[value] = { key }
            else
                table.insert(rtbl[value], key)
            end

            rawset(tbl, key, value)
        end,
    }
end

local tables = require("tables")

---------------------------------------------------
-- Bitable maps keys to values and vice-versa
---------------------------------------------------

---@class Bitable
local Bitable = {}

---@param tbl table
function Bitable:new(tbl)
    local impl = {}
    local rtbl = {}

    function impl:assign(key, new_value)
        if key == nil then
            return
        end

        local old_value = impl[key]

        if old_value ~= nil then
            tables.inplace_filter(rtbl[old_value], function (v) return v ~= key end)

            if #rtbl[old_value] == 0 then
                rtbl[old_value] = nil
            end

            impl[key] = nil
        end

        if new_value ~= nil then
            impl[key] = new_value
        end
    end

    function impl:reverse_lookup(value)
        return rtbl[value]
    end

    setmetatable(impl,
        MtComposer
        :init()
        .Class(self, impl)
        .Bitable(rtbl)
        :done()
    )

    for key, value in pairs(tbl) do
        impl:assign(key, value)
    end

    return impl
end

setmetatable(Bitable,
    MtComposer
    :init()
    .Callable(Bitable.new)
    :done()
)

return Bitable
