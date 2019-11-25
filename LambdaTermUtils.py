from functools import reduce
from itertools import product
from re import match, findall
import string


class Variable:
    def __init__(self, name):
        if not isinstance(name, str):
            raise TypeError("Variable name should be a string")

        elif not match(r"^[a-z]\d?$", name):
            raise SyntaxError("Variable name should contain a lowercase latin letter with or without a number")

        self.name = name

    def __str__(self):
        return self.name

    def __eq__(self, other):
        if not isinstance(other, Variable):
            raise ValueError("Invalid variable comparsion")

        return self.name == other.name


class Applicative:
    def __init__(self, fst, snd):
        if not ((isinstance(fst, Variable) or isinstance(fst, Applicative) or isinstance(fst, Lambda)) and (isinstance(snd, Variable) or isinstance(snd, Applicative) or isinstance(snd, Lambda))):
            raise SyntaxError("Invalid applicative construction")

        self.fst = fst
        self.snd = snd

    def __str__(self):
        result = ""

        if isinstance(self.fst, Applicative) or isinstance(self.fst, Lambda):
            result += f"({self.fst})"

        elif isinstance(self.fst, Variable):
            result += f"{self.fst}"

        if isinstance(self.snd, Applicative) or isinstance(self.snd, Lambda):
            result += f"({self.snd})"

        elif isinstance(self.snd, Variable):
            result += f"{self.snd}"

        return result


class Lambda:
    def __init__(self, bound_var, body):
        if not (isinstance(bound_var, Variable) and (isinstance(body, Variable) or isinstance(body, Applicative) or isinstance(body, Lambda))):
            raise SyntaxError("Invalid lambda construction")

        self.bound_var = bound_var
        self.body = body

    def __str__(self):
        return f"λ{self.bound_var}.{self.body}"


class TermUtils:
    @staticmethod
    def get_bound_variables(term):
        if isinstance(term, Lambda):
            return reduce(
                lambda acc, elem: acc+[elem] if not elem in acc else acc,
                [term.bound_var.name, *TermUtils.get_bound_variables(term.body)],
                []
            )

        elif isinstance(term, Applicative):
            return reduce(
                lambda acc, elem: acc+[elem] if not elem in acc else acc,
                [*TermUtils.get_bound_variables(term.fst), *TermUtils.get_bound_variables(term.snd)],
                []
            )

        elif isinstance(term, Variable):
            return []

    @staticmethod
    def get_unbound_variables(term, currently_bound=[]):
        if isinstance(term, Lambda):
            return TermUtils.get_unbound_variables(term.body, currently_bound=currently_bound + [term.bound_var])

        elif isinstance(term, Applicative):
            return reduce(
                    lambda acc, elem: acc+[elem] if not elem in acc else acc, 
                    [*TermUtils.get_unbound_variables(term.snd, currently_bound=currently_bound), *TermUtils.get_unbound_variables(term.fst, currently_bound=currently_bound)],
                    []
            )

        elif isinstance(term, Variable):
            if any(term.__eq__(v) for v in currently_bound):
                return []

            else:
                return [term.name]

    @staticmethod
    def sub(term):
        if isinstance(term, Lambda):
            return reduce(
                lambda acc, elem: acc+[elem] if not elem in acc else acc,
                [str(term), *TermUtils.sub(term.body)],
                []
            )

        elif isinstance(term, Applicative):
            return reduce(
                lambda acc, elem: acc+[elem] if not elem in acc else acc,
                [str(term), *TermUtils.sub(term.fst), *TermUtils.sub(term.snd)],
                []
            )

        elif isinstance(term, Variable):
            return [str(term)]

    @staticmethod
    def term_to_de_bruijin_code(term):
        unbound_var_map = {v: i for i, v in list(enumerate(list(TermUtils.get_unbound_variables(term))[::-1]))[::-1]}

        def iterate(it, var_map):
            if isinstance(it, Lambda):
                new_var_map = {v: i for i, v in list(enumerate((list(var_map.keys()) + [it.bound_var.name])[::-1]))[::-1]}

                if isinstance(it.body, Applicative):
                    if (isinstance(it.body.fst, Lambda) or isinstance(it.body.fst, Applicative)) and (isinstance(it.body.snd, Lambda) or isinstance(it.body.snd, Applicative)):
                        return f"λ.({iterate(it.body.fst, new_var_map)})({iterate(it.body.snd, new_var_map)})"

                    elif isinstance(it.body.fst, Variable) and (isinstance(it.body.snd, Lambda) or isinstance(it.body.snd, Applicative)):
                        return f"λ.{new_var_map[it.body.fst.name]}({iterate(it.body.snd, new_var_map)})"

                    elif (isinstance(it.body.fst, Lambda) or isinstance(it.body.fst, Applicative)) and isinstance(it.body.snd, Variable):
                        return f"λ.({iterate(it.body.fst, new_var_map)}){new_var_map[it.body.snd.name]}"

                    elif isinstance(it.body.fst, Variable) and isinstance(it.body.snd, Variable):
                        return f"λ.{new_var_map[it.body.fst.name]}{new_var_map[it.body.snd.name]}"

                elif isinstance(it.body, Lambda):
                    return f"λ.{iterate(it.body, new_var_map)}"

                elif isinstance(it.body, Variable):
                    return f"λ.{new_var_map[it.body.name]}"

            elif isinstance(it, Applicative):
                if (isinstance(it.fst, Lambda) or isinstance(it.fst, Applicative)) and (isinstance(it.snd, Lambda) or isinstance(it.snd, Applicative)):
                    return f"({iterate(it.fst, var_map)})({iterate(it.snd, var_map)})"

                elif isinstance(it.fst, Variable) and (isinstance(it.snd, Lambda) or isinstance(it.snd, Applicative)):
                    return f"{var_map[it.fst.name]}({iterate(it.snd, var_map)})"

                elif (isinstance(it.fst, Lambda) or isinstance(it.fst, Applicative)) and isinstance(it.snd, Variable):
                    return f"({iterate(it.fst, var_map)}){var_map[it.snd.name]}"

                elif isinstance(it.fst, Variable) and isinstance(it.snd, Variable):
                    return f"{var_map[it.fst.name]}{var_map[it.snd.name]}"

            elif isinstance(it, Variable):
                return f"{var_map[it.name]}"
        
        return "".join(["λ." for _ in range(len(list(unbound_var_map.keys())))]) + iterate(term, unbound_var_map)
    

if __name__ == "__main__":
    def test_actions(term):
        print("Term:", term)
        print("Unbound variables:", TermUtils.get_unbound_variables(term))
        print("Bound variables:", TermUtils.get_bound_variables(term))
        print("Sub-terms:", TermUtils.sub(term))
        print("Bruijin code:", TermUtils.term_to_de_bruijin_code(term))
        print()


    t1 = Applicative(
        Lambda(
            Variable("x"),
            Lambda(
                Variable("y"),
                Applicative(
                    Variable("x"),
                    Lambda(
                        Variable("z"),
                        Applicative(
                            Variable("y"),
                            Variable("z")
                        )
                    )
                )
            )
        ),
        Lambda(
            Variable("x"),
            Lambda(
                Variable("y"),
                Variable("z")
            )
        )
    )

    t2 = Applicative(
        Applicative(
            Variable("x1"),
            Variable("x2")
        ),
        Lambda(
            Variable("x3"),
            Applicative(
                Variable("x3"),
                Applicative(
                    Variable("x4"),
                    Variable("x2")
                )
            )
        )
    )

    t3 = Applicative(
        Lambda(
            Variable("x1"),
            Variable("x1")
        ),
        Applicative(
            Variable("x4"),
            Variable("x3")
        )
    )

    t4 = Applicative(
        Applicative(
            Lambda(
                Variable("x1"),
                Lambda(
                    Variable("x2"),
                    Applicative(
                        Variable("x2"),
                        Variable("x1")
                    )
                )
            ),
            Lambda(
                Variable("x3"),
                Variable("x1")
            )
        ),
        Applicative(
            Variable("m"),
            Variable("n")
        )
    )

    t5 = Lambda(
        Variable("x3"),
        Applicative(
            Lambda(
                Variable("x1"),
                Applicative(
                    Lambda(
                        Variable("x2"),
                        Variable("x2")
                    ),
                    Variable("x3")
                )
            ),
            Variable("x6")
        )
    )

    t6 = Applicative(
        Variable("a"),
        Variable("m")
    )

    t7 = Variable("a")

    t8 = Lambda(
        Variable("x"),
        Applicative(
            Applicative(
                Applicative(
                    Variable("x"),
                    Variable("x")
                ),
                Variable("x")
            ),
            Variable("x")
        )
    )


    tuple(map(test_actions, [t1, t2, t3, t4, t5, t6, t7, t8]))

