from typing import List

"""
  1 2 3 4 5
1 а б в г д
2 е ж з и к
3 л м н о п
4 р с т у ф
5 х ц ч ш щ
6 ь ы э ю я
"""

index_map = dict(
    zip(
        [
         'а', 'б', 'в', 'г', 'д',
         'е', 'ж', 'з', 'и', 'к',
         'л', 'м', 'н', 'о', 'п',
         'р', 'с', 'т', 'у', 'ф',
         'х', 'ц', 'ч', 'ш', 'щ',
         'ь', 'ы', 'э', 'ю', 'я'
        ],
        [
            j * 10 + i for j in range(1, 7) for i in range(1, 6)
        ]
    )
)

inverse_index_map = { v: k for k, v in index_map.items() }


def encode(in_str: str) -> List[int]:
    return list(
        map(
            lambda c: index_map[c],
            filter(
                lambda c: c.isalpha(),
                in_str.lower().replace("й", "и").replace("ё", "е").replace("ъ", "ь")
            )
        )
    )


def decode(in_nums: List[int]) -> str:
    return "".join(
        map(
            lambda num: inverse_index_map[num],
            in_nums
        )
    )

