local tables = {}

function tables.inplace_filter(t, pred)
    assert(type(t) == "table")
    assert(type(pred) == "function")

    local j = 1

    for i = 1, #t do
        local v = t[i]
        if pred(v) then
            t[j] = v
            j = j + 1
        end
    end

    for i = j, #t do
        t[i] = nil
    end
end

return tables
