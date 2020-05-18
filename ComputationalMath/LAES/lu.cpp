/*
 * =====================================================================================
 *
 *       Filename:  lu.cpp
 *
 *    Description:  Решение СЛАУ с помощью метода LU-разложения
 *    (Алгоритм Краута)
 *
 *        Version:  1.0
 *        Created:  11/21/2019 17:19:53
 *       Revision:  none
 *       Compiler:  g++
 *
 *         Author:  Вячеслав Беспалов 
 *
 * =====================================================================================
 */

#include <iostream>
using namespace std;

int main(void)
{
  int n;
  double sum = 0;

  cout << "Введите порядок матрицы\n n = ";
  cin >> n;

  double A [ n ][ n ];
  double B [ n ];
  double L [ n ][ n ];
  double U [ n ][ n ];

//задаем матрицу A[][] ...

  for (int i = 0; i < n; i++)
  {
    for (int j = 0; j < n; j++)
    {
      cout << "\na[" << i << "][" << j << "] = ";
      cin >> A [i][j];

      L [i][j] = 0;
      U [i][j] = 0;

      if (i == j)
        U [i][j] = 1;
    }
  }
  
  for (int i = 0; i < n; i++)
  {
    cout << "\nb[" << i << "] = ";
    cin >> B [i];
  }

//==============================================

//находим первый столбец L[][] и первую строку U[][]

  for (int i = 0; i < n; i++)
  {
    L [i][0] = A [i][0];
    U [0][i] = A [0][i] / L [0][0];
  }

//дальше вычисляем L[][], U[][] по формуле

  for (int i = 1; i < n; i++)
  {
      for (int j = 1; j < n; j++)
      {
          if (i >= j) //нижний треугольник
          {
              sum = 0;
              for (int k = 0; k < j; k++)
                  sum += L [i][k] * U [k][j];

              L [i][j] = A [i][j] - sum;
          }
          else // верхний
          {
              sum = 0;
              for (int k = 0; k < i; k++)
                  sum += L [i][k] * U [k][j];

              U [i][j] = (A [i][j] - sum) / L [i][i];
          }
      }
   }

//====================================================
   cout << "\n\n";

   for (int i = 0; i < n; i++)
   {
     for (int j = 0; j < n; j++)
       cout << "  " << L [i][j] << "   ";

     cout << "\n\n";
   }

   cout << "\n\n";

   for (int i = 0; i < n; i++)
   {
     for (int j = 0; j < n; j++)
       cout << "  " << U [i][j] << "   ";

     cout << "\n\n";
   }
  
   double f [ n ];
   for (int i = 0; i < n; i++) 
   {
     sum = 0;

     for (int j = 0; j < i; j++)
     {       
       sum += f [j] * L [i][j];
     }
     
     f [i] = (B [i] - sum) / L [i][i];
   }
  
  cout << "\n\nY:";
  for (int i = 0; i < n; i++) 
  {
    cout << "  " << f [i] << " ";
  }
  
  double x [ n ];
  for (int i = n - 1; i >= 0; i--) 
  {
    sum = 0;

    for (int j = n - 1; j > i; j--) 
    {
      sum += U [i][j] * x [j];
    }
    
    x [i] = f [i] - sum;
  }
  
  cout << "\n\nX:";
  for (int i = 0; i < n; i++) 
  {
    cout << "  " << x [i] << " ";
  }

   return 0;
}



