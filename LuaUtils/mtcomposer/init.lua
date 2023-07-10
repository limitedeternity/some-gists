---------------------------------------------------
-- MtComposer is used to compose metatables
---------------------------------------------------

---@class MtComposer
local MtComposer = {}

function MtComposer:init()
    local impl = {}
    local state = {}

    function impl:done()
        return state
    end

    setmetatable(impl, {
        __index = function (_, name)
            return function (...)
                local mt = self[name](...)

                for event, handler in pairs(mt) do
                    state[event] = handler
                end

                return impl
            end
        end,
    })

    return impl
end

function MtComposer.Callable(method)
    return {
        __call = method,
    }
end

function MtComposer.Class(super, this)
    this.__super = super

    return {
        __index = this.__super,
    }
end

return MtComposer
