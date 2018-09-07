using System;
using System.Configuration;
using System.Diagnostics;
using System.IO;
using System.Text.RegularExpressions;

public class SEFourMixedExp1
{
   //*********************************************************************
   //**           Calculate A1 for SE 4 Mixed Exp                   ***
   //*********************************************************************
   public static void SEFourMixedExpA1(double t1, double t2, double t3,
       double t4, double t5, double t6, double t7, double[,] A,
       ref int iterFlag)
   {
	   double Criteria = 0.0000000000000001;

      int maxIter = 100000;

      double test = 100.0;
      int k = 0;

      double t13P = Math.Pow(t1, 3);
      double t14P = Math.Pow(t1, 4);
      double t1aP = Math.Pow((1 + t1), (-3));
      double t1bP = Math.Pow(t1, (-2));

      //A11
      do
      {
         double t1P = Math.Pow((t1 / (1 + t1)), k);
         double t2P = Math.Pow((t2 / (1 + t2)), k);
         double t3P = Math.Pow((t3 / (1 + t3)), k);
         double t4P = Math.Pow((t4 / (1 + t4)), k);

         double t12kP = Math.Pow((t1 / (1 + t1)), (2 * k));
         double t22kP = Math.Pow((t2 / (1 + t2)), (2 * k));
         double t32kP = Math.Pow((t3 / (1 + t3)), (2 * k));
         double t42kP = Math.Pow((t4 / (1 + t4)), (2 * k));
         double t13kP = Math.Pow((t1 / (1 + t1)), (3 * k));

         double A11 = t5 * (4 * t6 * t6 * t22kP * t3 * t3 * t1P * k * t13P + 16 * t42kP * t6 * t14P * t3 * t1P * t2 - 2 * t42kP * t5 * t5 * t1P * k * k * t3 - 8
         * t42kP * t7 * t7 * t2 * t1P * k * k * t1 * t3 - 2 * t7 * t3P * t2 * t3 * t12kP * k * k * t5 - 48 * t42kP * t6 * t2 * t3 * t1P * k * t1 - 24 * t42kP
         * t3 * t5 * t1P * k * t1 * t2 * t2 - 8 * t42kP * t7 * t2 * t1P * k * t3 + 2 * t4P * t2 * t5 * t5 * t12kP * k * k * t4 - 8 * t6 * t6 * t22kP * t13P
         * t1P * t3 * t4 * t4 + 18 * t6 * t6 * t22kP * t3 * t1P * k * t1 * t1 + 4 * t42kP * t2 * t3 * t1P * k - 2 * t4P * t12kP * k * k * t1 * t5 * t3
         * t4 + 16 * t42kP * t6 * t13P * t3 * t1P * t2 * t2 + 9 * t6 * t6 * t22kP * t3 * t3 * t1P * k * t1 * t1 - 2 * t7 * t7 * t32kP * t1P * t14P
         + t4P * t6 * t12kP * k * k * t5 * t2 * t2 * t3 * t3 * t4 * t1 + 12 * t42kP * t2 * t2 * t5 * t1P * k * t1 * t6 + 16 * t42kP * t7 * t3 * t5 * t1P
         * k * t13P + 4 * t42kP * t1P * t1 * t1 * t5 - 2 * t42kP * t7 * t7 * t3 * t1P * k * k * t1 * t1 - 2 * t42kP * t6 * t6 * t1P * k * k * t1 * t2
         * t2 - t6 * t6 * t22kP * t3 * t3 * t1P * k * k * t1 * t1 - 4 * t42kP * t14P * t5 * t5 * t1P * t3 - 16 * t7 * t7 * t32kP * t13P * t2 * t4
         * t1P - 2 * t42kP * t2 * t2 * t5 * t5 * t1P * k * k * t1 - 2 * t4P * t2 * t2 * t3 * t4 * t12kP * k * k * t5 + t4P * t6 * t12kP * k * k * t5
         * t4 * t1 + 16 * t42kP * t3 * t5 * t1P * k * t13P * t6 + t4P * t7 * t12kP * k * k * t5 + 32 * t42kP * t7 * t3 * t5 * t1P * k * t13P
         * t2 + 2 * t42kP * t2 * t3 * t3 * t5 * t5 * t1P * k - 4 * t6 * t6 * t22kP * t3 * t1P * t1 * t1 - 2 * t6 * t6 * t22kP * t4 * t4 * t1P * t1 * t1
         * t3 * t3 - 2 * t6 * t2P * t3 * t3 * t4 * t12kP * k * k * t5 + 2 * t42kP * t2 * t2 * t5 * t1P * k * k * t3 * t3 + 4 * t42kP * t2 * t3 * t3 * t5 * t1P
         * k * t6 - 16 * t42kP * t3 * t5 * t1P * k * t13P * t2 * t2 - 2 * t7 * t3P * t2 * t2 * t3 * t4 * t12kP * k * k * t5 - 2 * t42kP * t5 * t1P
         * k * k * t1 * t1 * t6 * t3 * t3 + 16 * t42kP * t7 * t3 * t5 * t1P * k * t13P * t2 * t2 - 2 * t42kP * t7 * t2 * t2 * t1P * k + 8 * t42kP * t13P
         * t5 * t1P * t3 * t3 + 24 * t42kP * t7 * t3 * t5 * t1P * k * t1 * t2 * t2 - 2 * t7 * t3P * t12kP * k * k * t1 * t5 * t2 - 8 * t42kP * t13P
         * t1P * t2 * t3 * t3 + 4 * t42kP * t7 * t1P * t1 * t1 * t2 * t2 - 4 * t42kP * t6 * t6 * t1P * k * k * t2 * t3 * t1 * t1 - 4 * t42kP * t7 * t2
         * t1P * k * k * t1 * t1 * t6 * t3 * t3 - 2 * t42kP * t7 * t2 * t2 * t1P * k * t3 * t3 + 8 * t42kP * t7 * t2 * t1P * k * k * t1 + 36 * t6 * t6 * t22kP
         * t3 * t1P * k * t1 * t1 * t4 - 8 * t42kP * t2 * t5 * t1P * k * k * t6 * t1 - 16 * t42kP * t7 * t2 * t1P * k * t13P + 2 * t4P * t7 * t2 * t12kP
         * k * k * t1 * t5 * t4 + t42kP * t6 * t6 * t1P * k + 2 * t42kP * t2 * t1P * k - t42kP * t5 * t5 * t1P * k * k * t3 * t3 - 4 * t6 * t2P * t3
         * t12kP * k * k * t1 * t5 * t4 + t4P * t7 * t4 * t12kP * k * k * t5 + 2 * t4P * t6 * t12kP * k * k * t5 * t3 * t4 - 2 * t42kP * t14P * t5
         * t5 * t1P + 8 * t4P * t6 * t2 * t1P * t1 * t1 * t7 * t3P + 8 * t4P * t6 * t2 * t1P * t14P * t7 * t3P + 12 * t42kP * t7 * t2 * t2
         * t1P * k * t6 * t1 + 8 * t42kP * t6 * t13P * t3 * t3 * t1P + t4P * t7 * t3 * t3 * t12kP * k * k * t5 + 4 * t42kP * t1P * k * k * t1 * t7 - 2
         * t42kP * t6 * t1P * k * t2 * t2 * t3 * t3 + 4 * t42kP * t1P * k * t13P * t3 * t3 + 2 * t42kP * t7 * t2 * t2 * t1P * k * t5 - t6 * t2P
         * t12kP * k * k * t5 * t4 * t4 + 2 * t42kP * t2 * t2 * t3 * t3 * t5 * t1P * k * t7 + 4 * t7 * t7 * t32kP * t1P * k * t13P * t2 * t2 + 2 * t42kP
         * t5 * t1P * k * t6 + 2 * t42kP * t1P * k * k * t1 * t1 * t7 - 4 * t42kP * t6 * t3 * t3 * t1P * t1 * t1 * t7 + 36 * t42kP * t7 * t3 * t3 * t5
         * t1P * k * t1 * t1 * t2 - t4P * t2 * t2 * t3 * t3 * t4 * t12kP * k * k * t5 + 18 * t42kP * t6 * t6 * t2 * t3 * t3 * t1P * k * t1 * t1 - 32 * t42kP
         * t6 * t13P * t3 * t1P * t7 * t2 - 2 * t7 * t3P * t2 * t3 * t4 * t4 * t12kP * k * k * t5 - 36 * t42kP * t3 * t5 * t1P * k * t1 * t1 * t2 * t2 - 4
         * t42kP * t2 * t5 * t5 * t1P * k * k * t1 - 4 * t42kP * t6 * t1P * k * k * t7 * t3 * t1 * t1 + 8 * t42kP * t6 * t2 * t2 * t1P * t13P + 12
         * t42kP * t6 * t6 * t1P * k * t1 * t2 + 16 * t42kP * t14P * t5 * t1P * t2 * t3 + 4 * t42kP * t7 * t2 * t2 * t1P * k * k * t1 * t3 * t3 - 4
         * t42kP * t6 * t6 * t14P * t3 * t3 * t1P * t2 + 2 * t42kP * t5 * t1P * k * k * t1 * t1 + 2 * t4P * t7 * t4 * t12kP * k * k * t5 * t3 * t1 - 8
         * t42kP * t2 * t3 * t1P * t1 * t1 - 2 * t42kP * t2 * t1P * k * k * t1 * t1 - 4 * t42kP * t1P * k * k * t1 * t3 - 8 * t42kP * t14P * t5
         * t1P * t6 * t2 * t3 * t3 + 4 * t42kP * t1P * k * t13P - 8 * t42kP * t13P * t1P * t3 + t5 * t5 * t13kP * k * t2 * t2 + 8 * t42kP
         * t13P * t1P * t6 - 2 * t6 * t6 * t22kP * t1P * k * k * t1 - 16 * t42kP * t13P * t5 * t1P * t6 * t3 - 2 * t7 * t3P * t12kP * k * k
         * t1 * t5 * t2 * t3 * t4 * t4 - 4 * t42kP * t5 * t1P * k * k * t1 * t6 - 16 * t42kP * t7 * t2 * t1P * k * k * t1 * t6 * t3 + 18 * t42kP * t7 * t7
         * t2 * t2 * t1P * k * t3 * t1 * t1 - 4 * t42kP * t6 * t1P * k * k * t7 * t2 * t3 * t3 - 2 * t7 * t3P * t12kP * k * k * t5 * t2 - t7 * t3P * t12kP
         * k * k * t1 * t5 * t2 * t2 * t4 * t4 - 4 * t42kP * t7 * t2 * t2 * t1P * k * k * t1 * t6 * t3 * t3 - 8 * t7 * t7 * t32kP * t2 * t4 * t1P * t1 * t1 + 4 * t42kP
         * t6 * t1P * t1 * t1 - 2 * t6 * t6 * t22kP * t3 * t3 * t1P * k * k * t1 * t1 * t4 + 36 * t7 * t7 * t32kP * t1P * k * t1 * t1 * t2 * t4 - 2 * t42kP
         * t7 * t7 * t2 * t2 * t1P * k * k * t1 * t3 * t3 + 16 * t42kP * t7 * t7 * t2 * t1P * k * t13P * t3 - 4 * t42kP * t7 * t7 * t3 * t1P * k * k * t1
         + 9 * t7 * t7 * t32kP * t1P * k * t1 * t1 * t2 * t2 * t4 * t4 - 2 * t4P * t2 * t12kP * k * k * t5 - 2 * t42kP * t6 * t1P * k * k * t7 * t3 * t3 - 4
         * t42kP * t2 * t1P * k * k * t3 - t6 * t6 * t22kP * t3 * t3 * t1P * k * k * t1 * t1 * t4 * t4 + 12 * t42kP * t7 * t3 * t3 * t5 * t1P * k * t1 - 16
         * t42kP * t3 * t3 * t5 * t1P * k * t13P * t2 + t4P * t6 * t12kP * k * k * t5 * t2 * t2 * t3 * t3 * t4 - 4 * t42kP * t7 * t2 * t2 * t3 * t3 * t5
         * t1P * k * k * t1 - 2 * t42kP * t7 * t7 * t1P * t14P * t2 * t2 + 36 * t42kP * t5 * t5 * t3 * t1P * k * t1 * t1 * t2 - 4 * t7 * t7 * t32kP
         * t13P * t2 * t2 * t4 * t4 * t1P + 4 * t42kP * t6 * t6 * t1P * k * t13P * t2 * t2 - t42kP * t1P * k * k * t1 * t1 * t3 * t3 + t4P * t5
         * t5 * t12kP * k * k * t3 * t3 * t4 + 2 * t4P * t6 * t12kP * k * k * t5 * t2 + 18 * t42kP * t7 * t7 * t2 * t1P * k * t1 * t1 + 4 * t42kP * t5
         * t5 * t3 * t3 * t1P * k * t13P + 4 * t7 * t7 * t32kP * t1P * k * t13P + 2 * t5 * t5 * t13kP * k * t3 * t4 * t4 + 2 * t7 * t7 * t32kP * t2
         * t1P * k + t4P * t5 * t5 * t12kP * k * k * t1 - 4 * t42kP * t2 * t2 * t5 * t5 * t1P * t1 * t1 * t3 - 2 * t7 * t3P * t2 * t2 * t4 * t12kP
         * k * k * t5 + 24 * t42kP * t3 * t5 * t1P * k * t1 * t6 * t2 * t2 - 4 * t42kP * t6 * t6 * t13P * t3 * t3 * t1P - 4 * t42kP * t6 * t6 * t1P
         * k * k * t1 * t2 + 8 * t42kP * t1P * k * t13P * t2 + 4 * t6 * t6 * t22kP * t1P * k * t13P * t4 * t4 + 4 * t42kP * t6 * t1P * k * k * t1
         + 36 * t42kP * t6 * t3 * t1P * k * t1 * t1 * t7 - 2 * t7 * t3P * t12kP * k * k * t1 * t5 * t4 + 8 * t5 * t5 * t13kP * t2 * t4 * k * t3 - 8 * t42kP
         * t13P * t5 * t1P * t7 * t3 * t3 - 4 * t42kP * t6 * t6 * t14P * t3 * t1P + 4 * t42kP * t7 * t2 * t1P * k * t6 * t3 * t3 - 8 * t42kP
         * t7 * t1P * t1 * t1 * t5 * t2 - 2 * t42kP * t6 * t6 * t1P * k * k * t2 * t3 * t3 + 4 * t42kP * t6 * t6 * t1P * k * t13P - 8 * t42kP * t13P
         * t5 * t5 * t1P * t2 + 8 * t42kP * t7 * t7 * t2 * t2 * t1P * k * t13P * t3 + t4P * t6 * t12kP * k * k * t5 * t3 * t3 + 4 * t42kP * t2 * t2
         * t3 * t5 * t1P * k * t7 - t6 * t2P * t4 * t4 * t12kP * k * k * t1 * t5 - 8 * t42kP * t5 * t1P * k * k * t1 * t7 * t3 - 2 * t42kP * t2 * t2 * t5
         * t5 * t1P * k * k * t1 * t1 * t3 + 8 * t42kP * t7 * t2 * t3 * t3 * t1P * t14P + 4 * t4P * t2 * t5 * t5 * t12kP * k * k * t3 * t1 - 4 * t6 * t6
         * t22kP * t3 * t3 * t1P * k * k * t1 * t4 + 2 * t42kP * t5 * t1P * k * t7 * t3 * t3 - 4 * t6 * t6 * t22kP * t3 * t4 * t1P * k * k + 6 * t42kP
         * t1P * k * t1 * t2 * t2 - 4 * t42kP * t6 * t1P * k * k * t7 * t2 + 8 * t42kP * t13P * t5 * t1P * t2 * t2 * t3 * t3 + 24 * t42kP * t3 * t3
         * t5 * t1P * k * t1 * t6 * t2 + 4 * t5 * t5 * t13kP * k * t2 * t3 - t4P * t4 * t12kP * k * k * t5 - 4 * t42kP * t7 * t7 * t1P * t1 * t1 * t3 - t42kP
         * t7 * t7 * t2 * t2 * t1P * k * k * t1 * t1 * t3 * t3 + 8 * t42kP * t13P * t5 * t1P * t2 * t2 - 4 * t42kP * t5 * t1P * k * t3 + 2 * t42kP
         * t5 * t5 * t1P * k * t2 - 8 * t42kP * t13P * t5 * t1P * t6 * t2 * t2 + 8 * t42kP * t13P * t1P * t7 * t3 * t3 - 8 * t42kP * t7 * t2
         * t1P * k * k * t1 * t1 * t6 * t3 + 24 * t6 * t6 * t22kP * t3 * t1P * k * t1 * t4 - 4 * t6 * t6 * t22kP * t14P * t1P * t4 - 4 * t7 * t7 * t32kP
         * t1P * t13P * t2 * t2 + 4 * t4P * t7 * t3 * t12kP * k * k * t5 * t2 + 2 * t42kP * t2 * t2 * t3 * t3 * t5 * t1P * k * t6 - t4P * t2 * t2 * t12kP
         * k * k * t5 * t4 * t1 + 4 * t42kP * t2 * t5 * t1P * k * k * t3 * t3 * t1 * t1 + 24 * t42kP * t3 * t5 * t5 * t1P * k * t1 * t2 + 8 * t42kP * t7 * t2
         * t2 * t1P * k * k * t1 * t3 - t7 * t3P * t12kP * k * k * t5 * t4 * t4 - 8 * t42kP * t13P * t5 * t1P * t7 + 48 * t42kP * t3 * t5 * t1P
         * k * t1 * t6 * t2 - 2 * t42kP * t6 * t1P * k * t3 * t3 - t42kP * t1P * k * k + 12 * t42kP * t7 * t3 * t3 * t5 * t1P * k * t1 * t2 * t2 - 2 * t4P
         * t3 * t12kP * k * k * t5 - 8 * t42kP * t6 * t3 * t1P * t1 * t1 * t5 - 4 * t42kP * t5 * t1P * k * k * t6 * t3 + t4P * t2 * t2 * t5 * t5 * t12kP
         * k * k * t3 * t3 * t4 - t4P * t12kP * k * k * t1 * t5 * t3 * t3 - t7 * t7 * t32kP * t1P * k * k * t1 * t1 * t2 * t2 * t4 * t4 + 24 * t42kP * t2 * t5
         * t1P * k * t1 * t6 - t6 * t2P * t12kP * k * k * t5 * t2 - 8 * t42kP * t7 * t14P * t2 * t2 * t3 * t5 * t1P - 24 * t42kP * t7 * t2 * t1P
         * k * t1 - 2 * t42kP * t7 * t7 * t3 * t3 * t1P * k * k * t2 - 16 * t42kP * t7 * t13P * t2 * t2 * t3 * t5 * t1P - 2 * t42kP * t2 * t2 * t5 * t5
         * t1P * t1 * t1 * t3 * t3 + 8 * t42kP * t7 * t2 * t2 * t3 * t1P * t14P - 2 * t42kP * t6 * t6 * t1P * t14P + 8 * t42kP * t7 * t7 * t2
         * t1P * k * t13P * t3 * t3 - 8 * t42kP * t6 * t3 * t5 * t1P * k * k * t1 * t2 * t2 + 2 * t4P * t7 * t4 * t12kP * k * k * t5 * t3 - 8 * t42kP
         * t6 * t1P * k * k * t7 * t1 * t3 - 4 * t42kP * t6 * t6 * t1P * k * k * t2 * t3 + 6 * t7 * t7 * t32kP * t1P * k * t1 + 8 * t42kP * t5 * t1P
         * k * t13P * t7 + 8 * t42kP * t2 * t2 * t3 * t1P * k * t13P + 4 * t42kP * t14P * t5 * t1P * t3 * t3 + 8 * t42kP * t6 * t6 * t2 * t3
         * t3 * t1P * k * t13P + 4 * t42kP * t7 * t1P * t1 * t1 + 2 * t42kP * t6 * t1P * k * k - 2 * t6 * t2P * t3 * t12kP * k * k * t1 * t5 - 4
         * t42kP * t2 * t2 * t5 * t1P * k * k * t6 * t1 + 36 * t42kP * t2 * t5 * t1P * k * t1 * t1 * t7 - 16 * t42kP * t6 * t13P * t3 * t1P * t7
         + 4 * t42kP * t2 * t2 * t1P * k * k * t6 * t1 - t6 * t2P * t2 * t3 * t3 * t12kP * k * k * t5 + 18 * t42kP * t5 * t1P * k * t1 * t1 * t6 + 8 * t42kP
         * t7 * t1P * t1 * t1 * t2 + 8 * t42kP * t6 * t2 * t2 * t1P * t1 * t1 * t3 - 2 * t42kP * t5 * t1P * k * k * t7 * t3 * t3 - 2 * t6 * t2P * t3 * t12kP
         * k * k * t1 * t5 * t2 * t4 * t4 + 2 * t42kP * t5 * t1P * k * k - 4 * t42kP * t5 * t1P * k * k * t1 * t1 * t6 * t3 + 12 * t7 * t7 * t32kP * t2 * t1P
         * k * t1 * t4 * t4 - 16 * t42kP * t7 * t14P * t2 * t3 * t5 * t1P + 4 * t4P * t7 * t2 * t12kP * k * k * t1 * t5 * t3 * t4 - 4 * t42kP * t2 * t5
         * t1P * k * k * t6 * t1 * t1 - 4 * t42kP * t6 * t1P * k * k * t7 * t3 - 4 * t42kP * t14P * t5 * t5 * t1P * t2 - 2 * t7 * t3P * t12kP * k
         * k * t1 * t5 * t2 * t4 * t4 - 4 * t42kP * t13P * t1P * t2 * t2 - 2 * t42kP * t2 * t1P * k * k + t42kP * t7 * t7 * t1P * k + t5 * t5 * t13kP
         * k * t4 * t4 + 2 * t5 * t5 * t13kP * t2 * t4 * t4 * k * t3 * t3 - 8 * t42kP * t7 * t7 * t1P * t13P * t3 + 9 * t42kP * t7 * t7 * t2 * t2 * t1P
         * k * t1 * t1 - 2 * t4P * t2 * t3 * t3 * t4 * t12kP * k * k * t5 * t1 + 12 * t42kP * t7 * t7 * t2 * t1P * k * t1 * t3 * t3 - 8 * t6 * t6 * t22kP * t14P
         * t1P * t3 * t4 - 8 * t42kP * t13P * t5 * t5 * t1P * t3 - 4 * t42kP * t6 * t6 * t13P * t3 * t3 * t1P * t2 * t2 - 8 * t42kP * t6 * t6
         * t1P * k * k * t1 * t2 * t3 + 2 * t42kP * t2 * t2 * t5 * t1P * k * k * t3 * t3 * t1 * t1 - 8 * t42kP * t6 * t14P * t3 * t3 * t1P * t7 * t2 + 16
         * t42kP * t2 * t3 * t1P * t1 * t1 * t5 - 4 * t42kP * t5 * t1P * k * t2 + 4 * t42kP * t7 * t3 * t1P * k * k - 8 * t42kP * t7 * t14P * t2
         * t3 * t3 * t5 * t1P + 12 * t6 * t6 * t22kP * t3 * t3 * t1P * k * t1 * t4 - 2 * t42kP * t6 * t2 * t2 * t3 * t3 * t5 * t1P * k * k - 4 * t42kP
         * t6 * t2 * t2 * t3 * t5 * t1P * k * k + 36 * t42kP * t7 * t2 * t1P * k * t6 * t3 * t3 * t1 * t1 - 24 * t42kP * t3 * t5 * t1P * k * t1 - 2 * t7 * t7
         * t32kP * t1P * k * k * t2 - 4 * t7 * t7 * t32kP * t1P * t14P * t2 - 4 * t7 * t7 * t32kP * t2 * t4 * t4 * t1P * t1 * t1 + 36 * t42kP
         * t3 * t5 * t1P * k * t1 * t1 * t6 * t2 * t2 + 9 * t42kP * t5 * t5 * t3 * t3 * t1P * k * t1 * t1 * t2 * t2 + t5 * t5 * t13kP * t2 * t2 * t4 * t4 * k - 4
         * t42kP * t7 * t7 * t1P * t14P * t2 - 8 * t42kP * t1P * k * t13P * t6 * t2 * t2 - 2 * t42kP * t6 * t6 * t1P * k * k * t3 - 2 * t42kP
         * t2 * t2 * t5 * t1P * k * k * t6 - 2 * t42kP * t5 * t1P * k * k * t1 * t1 * t7 * t3 * t3 - 4 * t42kP * t2 * t2 * t3 * t1P * t1 * t1 - t6 * t2P
         * t2 * t4 * t4 * t12kP * k * k * t5 - t6 * t6 * t22kP * t3 * t3 * t4 * t4 * t1P * k * k + 8 * t42kP * t2 * t3 * t5 * t1P * k * t7 + 24 * t42kP
         * t7 * t2 * t1P * k * t6 * t1 + 18 * t42kP * t3 * t3 * t5 * t1P * k * t1 * t1 * t6 * t2 * t2 + 8 * t42kP * t7 * t7 * t1P * k * t13P * t3 + 2 * t4P
         * t7 * t2 * t4 * t12kP * k * k * t5 + 18 * t42kP * t7 * t3 * t3 * t5 * t1P * k * t1 * t1 * t2 * t2 + 16 * t42kP * t7 * t2 * t2 * t1P * k * t6 * t13P
         * t3 + 2 * t5 * t5 * t13kP * t2 * t2 * t4 * k - t6 * t2P * t12kP * k * k * t5 * t3 * t3 + 18 * t7 * t7 * t32kP * t1P * k * t1 * t1 * t2 - 8 * t42kP
         * t13P * t5 * t1P * t6 - 36 * t42kP * t7 * t2 * t1P * k * t3 * t3 * t1 * t1 - 2 * t42kP * t14P * t1P * t2 * t2 + t7 * t7 * t32kP * t1P
         * k - t42kP * t5 * t5 * t1P * k * k + 2 * t5 * t5 * t13kP * k * t2 + 2 * t6 * t6 * t22kP * t4 * t1P * k * t3 * t3 + 6 * t42kP * t7 * t7 * t1P
         * k * t1 * t3 * t3 - 4 * t42kP * t7 * t14P * t2 * t2 * t3 * t3 * t5 * t1P - 2 * t42kP * t7 * t7 * t1P * t1 * t1 + t42kP * t2 * t2 * t1P
         * k - 2 * t6 * t6 * t22kP * t14P * t1P - 2 * t42kP * t7 * t7 * t1P * t14P + 16 * t42kP * t13P * t5 * t1P * t2 * t2 * t3 - 16 * t42kP
         * t1P * k * t13P * t7 * t3 - t42kP * t7 * t7 * t3 * t3 * t1P * k * k + 2 * t4P * t7 * t2 * t2 * t3 * t4 * t12kP * k * k * t5 + 2 * t4P * t2
         * t5 * t5 * t12kP * k * k * t3 * t3 + 12 * t42kP * t1P * k * t1 * t2 - 8 * t42kP * t6 * t2 * t3 * t5 * t1P * k * k + 36 * t42kP * t7 * t2 * t2
         * t1P * k * t6 * t3 * t1 * t1 + 8 * t42kP * t2 * t3 * t5 * t1P * k * t6 + 18 * t42kP * t2 * t5 * t5 * t1P * k * t1 * t1 - 2 * t42kP * t5 * t1P
         * k * k * t1 * t1 * t6 + 32 * t42kP * t13P * t5 * t1P * t2 * t3 - 4 * t42kP * t2 * t3 * t3 * t5 * t1P * k + 16 * t42kP * t13P * t5 * t1P
         * t3 + 4 * t42kP * t14P * t1P * t6 - 8 * t42kP * t7 * t7 * t2 * t3 * t1P * t1 * t1 - 2 * t42kP * t6 * t1P * k * k * t7 * t1 * t1 + 8 * t42kP
         * t1P * t1 * t1 * t5 * t3 - t7 * t7 * t32kP * t1P * k * k * t1 * t1 * t4 * t4 + 48 * t42kP * t7 * t2 * t1P * k * t6 * t1 * t3 - t7 * t3P * t2
         * t2 * t4 * t4 * t12kP * k * k * t5 + 4 * t42kP * t2 * t2 * t5 * t1P * k * k * t3 - t42kP * t7 * t7 * t2 * t2 * t1P * k * k + 4 * t42kP * t5 * t1P
         * k * k * t1 + 2 * t42kP * t7 * t1P * k * k + 8 * t42kP * t7 * t2 * t1P * k * k * t1 * t3 * t3 - t7 * t3P * t12kP * k * k * t5 * t3 * t4 * t4 - 2
         * t4P * t2 * t3 * t3 * t4 * t12kP * k * k * t5 + 6 * t6 * t6 * t22kP * t3 * t3 * t1P * k * t1 * t4 * t4 - 4 * t42kP * t14P * t1P * t2 * t3
         * t3 + 8 * t42kP * t6 * t6 * t1P * k * t13P * t2 - 4 * t42kP * t6 * t1P * k * t2 - 2 * t42kP * t2 * t2 * t5 * t5 * t1P * k * k * t1 * t3 * t3
         + 2 * t42kP * t2 * t2 * t1P * k * k * t6 + 4 * t42kP * t5 * t1P * k * k * t3 - 2 * t42kP * t2 * t2 * t1P * t1 * t1 + t4P * t2 * t2 * t5 * t5
         * t12kP * k * k * t3 * t3 * t4 * t1 + 2 * t4P * t2 * t5 * t5 * t12kP * k * k * t3 * t3 * t1 + t4P * t6 * t12kP * k * k * t5 * t2 * t2 * t4 * t1 - 8
         * t42kP * t6 * t13P * t3 * t3 * t1P * t7 - 32 * t42kP * t13P * t5 * t1P * t6 * t2 * t3 + 12 * t42kP * t6 * t3 * t3 * t1P * k * t1
         * t7 - 8 * t42kP * t13P * t5 * t5 * t1P * t2 * t3 * t3 + 2 * t42kP * t5 * t5 * t1P * k * t3 + 2 * t4P * t7 * t3 * t12kP * k * k * t5 + 2 * t42kP
         * t2 * t2 * t5 * t1P * k * k * t1 * t1 + 18 * t42kP * t2 * t2 * t3 * t1P * k * t1 * t1 + 12 * t42kP * t7 * t7 * t2 * t2 * t1P * k * t1 * t3 + 4 * t42kP
         * t7 * t2 * t2 * t3 * t3 * t1P * t14P - 16 * t42kP * t6 * t2 * t1P * t1 * t1 * t7 * t3 - 16 * t42kP * t13P * t1P * t2 * t3 - 4 * t7 * t7
         * t32kP * t14P * t2 * t4 * t4 * t1P + 2 * t42kP * t5 * t1P * k * t6 * t2 * t2 - 2 * t6 * t6 * t22kP * t14P * t1P * t3 * t3 * t4 * t4
         + 4 * t42kP * t2 * t1P * k * k * t7 * t3 * t3 - 2 * t4P * t2 * t12kP * k * k * t5 * t3 * t3 + 4 * t42kP * t2 * t2 * t1P * k * k * t7 * t3 + 12 * t42kP
         * t7 * t7 * t2 * t1P * k * t1 + 6 * t7 * t7 * t32kP * t2 * t2 * t1P * k * t1 - 2 * t6 * t2P * t12kP * k * k * t5 * t3 - 8 * t42kP * t7 * t2 * t2
         * t3 * t1P * t1 * t1 * t5 + 8 * t42kP * t7 * t2 * t2 * t3 * t3 * t1P * t13P + 4 * t4P * t2 * t5 * t5 * t12kP * k * k * t3 * t4 - t7 * t3P
         * t12kP * k * k * t1 * t5 * t3 * t4 * t4 - 8 * t42kP * t6 * t6 * t13P * t3 * t1P * t2 * t2 + 4 * t4P * t6 * t12kP * k * k * t5 * t2 * t3 * t1 - t42kP
         * t6 * t6 * t1P * k * k * t2 * t2 * t1 * t1 + 8 * t42kP * t7 * t3 * t3 * t5 * t1P * k * t13P + t4P * t6 * t12kP * k * k * t5 * t4 + t4P * t6
         * t12kP * k * k * t5 * t2 * t2 * t1 - 2 * t42kP * t6 * t1P * k * k * t7 * t2 * t2 * t3 * t3 - t42kP * t2 * t2 * t5 * t5 * t1P * k * k * t1 * t1 * t3
         * t3 - 8 * t42kP * t14P * t1P * t2 * t3 + 8 * t42kP * t7 * t2 * t3 * t3 * t1P * t1 * t1 - 4 * t42kP * t14P * t1P * t2 - 4 * t42kP
         * t7 * t7 * t1P * t14P * t3 - 4 * t6 * t6 * t22kP * t13P * t1P * t4 * t4 + 8 * t42kP * t1P * k * t13P * t3 + 2 * t42kP * t6 * t6
         * t1P * k * t2 * t3 * t3 - 4 * t42kP * t2 * t5 * t1P * k * k * t6 - t7 * t3P * t2 * t2 * t3 * t4 * t4 * t12kP * k * k * t5 - t5 * t5 * t13kP * t1
         * t1 + 4 * t42kP * t2 * t1P * k * k * t6 * t3 * t3 * t1 * t1 + 2 * t6 * t6 * t22kP * t4 * t1P * k - 2 * t42kP * t2 * t2 * t5 * t1P * k * k * t7 - 8
         * t7 * t7 * t32kP * t1P * t13P * t2 - 4 * t42kP * t6 * t6 * t1P * k * k * t1 * t3 + 18 * t42kP * t6 * t3 * t3 * t1P * k * t1 * t1 * t7 - 2
         * t6 * t6 * t22kP * t3 * t1P * k * k * t1 * t1 * t4 * t4 + t42kP * t3 * t3 * t1P * k + 8 * t42kP * t7 * t2 * t1P * k * k * t1 * t1 * t3 + 4 * t42kP
         * t14P * t1P * t7 * t3 * t3 + 4 * t5 * t12kP * k * t7 * t3P * t2 * t3 + 2 * t7 * t3P * t1P * k * k * t1 * t1 * t4P * t5 * t3 * t4 - 2 * t7 * t3P
         * t1P * k * k * t1 * t1 * t4P * t3 * t4 - 4 * t4P * t14P * t1P * t7 * t3P - 16 * t4P * t13P * t1P * t7 * t3P * t2 - 8 * t4P
         * t13P * t1P * t7 * t3P * t4 - 6 * t5 * t5 * t12kP * t2 * t2 * t3 * t3 * k * t1 * t1 * t4P * t4 + 4 * t4P * t7 * t1P * t1 * t1 * t6 * t2P
         * t3 * t3 * t4 - 12 * t4P * t7 * t7 * t1P * k * t1 * t3P * t3 + 8 * t6 * t2P * t14P * t1P * t4P * t7 * t3 * t4 - 4 * t6 * t2P * t14P
         * t1P * t4P * t2 - 8 * t6 * t2P * t14P * t1P * t4P * t3 - 16 * t6 * t2P * t13P * t1P * t4P * t2 * t3 - 4 * t6 * t2P * t14P
         * t1P * t7 * t3P - 8 * t6 * t2P * t13P * t1P * t7 * t3P * t2 - 16 * t6 * t2P * t13P * t1P * t7 * t3P * t4 + 12 * t5 * t12kP
         * t13P * t4P * t6 * t2 * t3 - 8 * t4P * t7 * t2 * t1P * k * t6 * t2P * t13P + 8 * t7 * t3P * t5 * t12kP * k * t1 * t2 * t2 - 16 * t5 * t5
         * t12kP * t3 * t3 * t4 * k * t1 * t4P * t2 - 6 * t5 * t5 * t12kP * t3 * t3 * t4 * k * t1 * t1 * t4P - 12 * t5 * t12kP * t2 * t2 * t3 * k * t1 * t1
         * t4P * t7 + 6 * t5 * t12kP * t1 * t1 * t4P * t7 * t2 * t2 * t3 * t4 + 6 * t5 * t12kP * t1 * t1 * t4P * t6 * t2 * t3 * t3 * t4 - 6 * t5 * t12kP
         * t1 * t1 * t7 * t3P * t2 * t4 * t4 * t3 + 3 * t5 * t5 * t12kP * t1 * t1 * t4P * t3 * t3 + 3 * t5 * t12kP * t1 * t1 * t4P * t6 * t3 * t3 + 12 * t5
         * t12kP * t3 * t4 * k * t1 * t1 * t4P + 32 * t5 * t12kP * t3 * t4 * k * t1 * t4P * t2 - 2 * t6 * t2P * t2 * t4 * t1P * k * k * t4P + 36 * t7
         * t3P * t1P * k * t1 * t1 * t6 * t2P * t4 * t2 - 18 * t7 * t3P * t3 * t5 * t1P * k * t1 * t1 * t4P * t2 * t2 - 8 * t7 * t3P * t14P * t3
         * t1P * t6 * t2P * t4 * t2 + 4 * t6 * t2P * t14P * t1P * t4P * t7 * t3 * t3 * t4 - 8 * t6 * t2P * t13P * t1P * t7 * t3P * t2 * t4
         * t4 + 2 * t6 * t2P * t1P * k * k * t4P * t7 * t4 - 2 * t7 * t3P * t1P * k * k * t1 * t1 * t6 * t2P - 8 * t7 * t3P * t1P * k * k * t1 * t6 * t2P
         * t4 - 4 * t7 * t3P * t4 * t4 * t1P * t1 * t1 * t6 * t2P - 8 * t7 * t3P * t4 * t1P * t1 * t1 * t6 * t2P + 4 * t7 * t3P * t1P * k * k * t1
         * t4P * t6 + 4 * t7 * t3P * t1P * k * k * t1 * t4P * t5 + 12 * t6 * t2P * t3 * t1P * k * t1 * t7 * t3P - 8 * t4P * t6 * t4 * t1P * k * t13P
         * t7 * t3P * t2 * t2 - 3 * t5 * t12kP * t1 * t1 * t7 * t3P * t2 * t2 * t3 - 6 * t5 * t12kP * t1 * t1 * t6 * t2P * t3 - 6 * t5 * t12kP * t1 * t1
         * t6 * t2P * t4 - 6 * t5 * t12kP * t1 * t1 * t7 * t3P * t2 * t2 * t4 * t3 - 3 * t5 * t12kP * t1 * t1 * t6 * t2P - 3 * t5 * t12kP * t1 * t1 * t7
         * t3P + 6 * t5 * t12kP * t1 * t1 * t4P * t6 * t2 - 12 * t5 * t12kP * t2 * t3 * t3 * k * t1 * t1 * t4P * t7 - 16 * t4P * t4 * t5 * t1P * k
         * t13P * t6 * t2P * t3 * t2 - 8 * t4P * t4 * t5 * t1P * k * t13P * t6 * t2P * t2 + 12 * t4P * t5 * t5 * t12kP * t2 * t1 * t1 * t3 + 3
         * t4P * t5 * t5 * t12kP * t2 * t2 * t1 * t1 + 6 * t4P * t5 * t5 * t12kP * t2 * t2 * t1 * t1 * t3 + 6 * t4P * t5 * t5 * t12kP * t2 * t13P
         + 12 * t4P * t5 * t5 * t12kP * t2 * t13P * t3 + 3 * t4P * t5 * t5 * t12kP * t2 * t2 * t13P + 6 * t4P * t5 * t5 * t12kP * t2 * t2 * t13P
         * t3 + 16 * t4P * t7 * t7 * t13P * t2 * t4 * t1P * t3P + 3 * t5 * t5 * t12kP * t13P * t4P * t3 * t3 + 8 * t4P * t6 * t13P * t3 * t1P
         * t7 * t3P - 8 * t4P * t4 * t1P * t1 * t1 * t6 * t2P * t3 - 4 * t4P * t4 * t1P * t1 * t1 * t6 * t2P - 4 * t4P * t4 * t1P * t1 * t1 * t7
         * t3P + 4 * t4P * t6 * t6 * t3 * t3 * t1P * t1 * t1 * t2P + 8 * t4P * t6 * t6 * t3 * t1P * t1 * t1 * t2P * t4 + 6 * t5 * t12kP * t1 * t1
         * t4P * t7 * t2 * t3 * t3 * t4 - 8 * t4P * t7 * t5 * t12kP * k * t4 * t1 - 6 * t4P * t7 * t5 * t12kP * k * t4 * t1 * t1 + 3 * t5 * t12kP * t13P
         * t4 * t4P * t6 * t3 * t3 - 6 * t5 * t12kP * t13P * t4 * t4 * t7 * t3P * t2 * t3 + 16 * t7 * t3P * t5 * t12kP * k * t1 * t2 * t2 * t4 + 6 * t7
         * t3P * t5 * t12kP * k * t1 * t1 * t2 * t2 + 12 * t7 * t3P * t5 * t12kP * k * t1 * t1 * t2 * t2 * t4 + 4 * t7 * t7 * t3P * t2 * t2 * t4 * t1P
         * t1 * t1 * t4P - 12 * t5 * t12kP * t2 * t3 * t3 * k * t1 * t1 * t4P * t6 + 2 * t6 * t2P * t1P * k * k * t4P * t7 + 8 * t5 * t12kP * t2 * t2
         * k * t1 * t4P * t3 * t3 + 8 * t5 * t12kP * t2 * k * t1 * t6 * t2P * t3 * t3 + 2 * t7 * t3P * t1P * k * k * t1 * t1 * t4P * t6 * t2 * t2 - 16 * t4P
         * t6 * t6 * t4 * t1P * k * t13P * t2P * t3 * t2 + 18 * t7 * t3P * t1P * k * t1 * t1 * t4P * t2 * t2 * t4 - 2 * t6 * t6 * t2P * t4 * t1P
         * k * t4P * t3 * t3 - 3 * t5 * t12kP * t13P * t6 * t2P - 3 * t5 * t12kP * t13P * t7 * t3P - 2 * t6 * t2P * t3 * t4 * t4 * t1P * k
         * k * t7 * t3P - 36 * t6 * t2P * t3 * t5 * t1P * k * t1 * t1 * t4P + 3 * t5 * t12kP * t13P * t4P * t6 * t2 * t2 * t3 * t3 - 6 * t5 * t12kP
         * t13P * t6 * t2P * t3 + 24 * t5 * t12kP * t2 * t3 * k * t1 * t1 * t4P * t4 - 16 * t4P * t7 * t5 * t12kP * k * t2 * t3 * t3 * t1 - 2 * t4P
         * t7 * t2 * t1P * k * t6 * t2P * t3 * t3 * t4 - 2 * t4P * t7 * t7 * t2 * t2 * t1P * k * t3P * t4 * t3 - 18 * t4P * t7 * t2 * t1P * k * t6 * t2P
         * t3 * t3 * t1 * t1 + 4 * t6 * t2P * t2 * t1P * t1 * t1 * t4P * t7 * t4 + 8 * t4P * t6 * t6 * t3 * t1P * t1 * t1 * t2P + 4 * t4P * t6 * t3 * t1P
         * t1 * t1 * t7 * t3P - 8 * t5 * t5 * t13kP * t2 * t1 * t1 * t3 * t4 + 8 * t7 * t3P * t1P * k * k * t1 * t4P * t6 * t2 * t3 * t4 - 4 * t7 * t3P * t1P
         * k * k * t1 * t1 * t6 * t2P * t4 * t2 - 3 * t4P * t5 * t12kP * t3 * t3 * t1 * t1 - 12 * t7 * t3P * t5 * t12kP * t2 * t1 * t1 * t4 - 6 * t7 * t3P
         * t5 * t12kP * t2 * t13P - 2 * t5 * t5 * t13kP * t4 * t4 * t1 * t1 * t3 + 4 * t5 * t5 * t13kP * k * t2 * t3 * t3 * t4 * t4 * t1 + 4 * t5 * t5 * t13kP
         * k * t2 * t2 * t3 * t4 * t4 * t1 + 16 * t4P * t2 * t3 * t1P * k * t13P * t6 * t2P + 8 * t4P * t6 * t1P * t13P * t7 * t3P - 12 * t4P
         * t7 * t2 * t1P * k * t6 * t2P * t1 * t3 * t3 - 2 * t4P * t5 * t1P * k * t7 * t3P * t3 + 8 * t4P * t7 * t13P * t2 * t4 * t1P * t6 * t2P - 12
         * t5 * t12kP * t3 * k * t1 * t1 * t4P * t6 - 12 * t5 * t12kP * t3 * k * t1 * t1 * t4P * t7 + 4 * t4P * t2 * t4 * t5 * t1P * t14P * t6 * t2P
         + 8 * t4P * t2 * t4 * t5 * t1P * t14P * t7 * t3P + 4 * t7 * t3P * t1P * k * k * t1 * t1 * t4P * t6 * t2 * t3 * t4 - 16 * t4P * t6 * t6 * t2
         * t3 * t1P * k * t13P * t2P - 4 * t5 * t5 * t13kP * t2 * t2 * t1 * t1 * t3 * t4 - 18 * t4P * t7 * t7 * t2 * t2 * t1P * k * t3P * t1 * t1 * t3 - 36
         * t6 * t2P * t3 * t1P * k * t1 * t1 * t4P * t7 * t4 + 8 * t4P * t6 * t6 * t13P * t3 * t3 * t1P * t2P * t4 * t2 - 12 * t4P * t6 * t3 * t1P
         * k * t1 * t7 * t3P - 8 * t6 * t2P * t3 * t1P * t1 * t1 * t4P + 8 * t4P * t5 * t12kP * k * t4 * t1 + 16 * t6 * t2P * t13P * t1P * t4P
         * t7 * t2 * t3 - 8 * t6 * t2P * t13P * t1P * t4P * t3 * t3 * t4 - 4 * t6 * t2P * t14P * t1P * t4P * t2 * t3 * t3 * t4 - 3 * t4P * t5
         * t12kP * t4 * t1 * t1 + 2 * t6 * t6 * t2P * t2 * t4 * t1P * k * k * t4P * t3 * t3 + 4 * t7 * t3P * t1P * k * k * t1 * t4P * t6 * t2 * t2 * t3
         + 3 * t5 * t12kP * t1 * t1 * t4P * t7 - 6 * t5 * t12kP * t1 * t1 * t6 * t2P * t3 * t3 * t4 * t2 + 3 * t5 * t12kP * t1 * t1 * t4P * t6 - 8 * t6
         * t2P * t13P * t1P * t4P * t2 - 8 * t6 * t2P * t13P * t1P * t4P * t2 * t3 * t3 - 4 * t6 * t2P * t14P * t1P * t4P * t3
         * t3 + 8 * t4P * t7 * t1P * t1 * t1 * t6 * t2P * t3 * t4 - 4 * t6 * t2P * t2 * t4 * t1P * k * k * t7 * t3P + 4 * t5 * t5 * t13kP * k * t3 * t1
         + 2 * t5 * t5 * t13kP * k * t3 * t3 * t1 + 8 * t5 * t5 * t13kP * k * t3 * t1 * t4 + 12 * t6 * t2P * t1P * k * t1 * t7 * t3P * t4 * t4 - 8 * t6 * t2P
         * t1P * k * t13P * t4P * t7 - 8 * t6 * t6 * t2P * t1P * k * t13P * t4P + 8 * t5 * t12kP * t2 * t4 * k * t6 * t2P * t3 + 2 * t5 * t12kP
         * t2 * t4 * t4 * k * t6 * t2P + 6 * t5 * t12kP * t2 * t2 * t4 * k * t4P * t1 * t1 + 4 * t6 * t2P * t14P * t1P * t4P * t7 + 8 * t6 * t2P
         * t13P * t1P * t4P * t7 * t2 + 4 * t6 * t6 * t2P * t14P * t1P * t4P - 8 * t6 * t2P * t13P * t1P * t4P - 4 * t4P * t14P
         * t1P * t7 * t3P * t2 * t2 - 6 * t4P * t5 * t12kP * t4 * t1 * t1 * t2 - 6 * t4P * t5 * t12kP * t4 * t13P * t3 - 12 * t6 * t6 * t2P * t1P
         * k * t1 * t4P - 12 * t6 * t2P * t1P * k * t1 * t4P * t5 - 4 * t6 * t2P * t3 * t3 * t1P * t1 * t1 * t4P + 4 * t6 * t2P * t3 * t3 * t1P
         * t1 * t1 * t4P * t5 - 12 * t7 * t3P * t2 * t2 * t1P * k * t1 * t4P * t6 * t4 + 3 * t4P * t7 * t5 * t12kP * t4 * t1 * t1 - 6 * t4P * t5 * t12kP
         * t4 * t1 * t1 * t3 + 4 * t6 * t2P * t4 * t1P * k * t7 * t3P - 2 * t6 * t6 * t2P * t4 * t1P * k * t4P * t2 - 6 * t5 * t12kP * k * t1 * t1 * t4P
         * t6 - 12 * t6 * t6 * t2P * t2 * t1P * k * t1 * t4P - 4 * t4P * t14P * t1P * t7 * t3P * t2 * t2 * t4 + 16 * t4P * t7 * t7 * t1P * t13P
         * t3P * t2 + 4 * t6 * t2P * t2 * t3 * t1P * k * k * t4P * t7 + 16 * t6 * t2P * t13P * t1P * t4P * t7 * t3 - 4 * t6 * t2P * t14P
         * t1P * t4P * t4 - 8 * t6 * t2P * t13P * t1P * t4P * t3 * t3 + 16 * t7 * t3P * t1P * k * t13P * t4P * t2 * t3 - 12 * t4P * t5
         * t12kP * t4 * t1 * t1 * t2 * t3 - 3 * t4P * t5 * t12kP * t4 * t13P - 6 * t4P * t5 * t12kP * t4 * t13P * t2 + 4 * t7 * t7 * t3P * t1P
         * k * k * t4P * t2 + 2 * t7 * t3P * t1P * k * k * t4P * t6 * t3 + 2 * t7 * t3P * t1P * k * k * t4P * t5 * t3 + 2 * t7 * t7 * t3P * t1P * k
         * k * t4P * t3 - 2 * t7 * t3P * t1P * k * k * t4P + 3 * t5 * t12kP * t13P * t4P * t6 * t2 * t2 + 3 * t5 * t12kP * t13P * t4P * t7
         * t2 * t2 * t3 * t3 - 3 * t5 * t12kP * t13P * t6 * t2P * t2 + 12 * t5 * t12kP * k * t1 * t1 * t6 * t2P * t4 + 6 * t5 * t12kP * k * t1 * t1 * t6
         * t2P * t4 * t4 + 12 * t5 * t12kP * t1 * t1 * t4P * t7 * t2 * t3 - 3 * t5 * t12kP * t1 * t1 * t4P * t3 * t3 * t4 + 2 * t7 * t3P * t2 * t2 * t1P
         * k * t4P + 4 * t7 * t3P * t2 * t1P * k * t4P + 2 * t6 * t2P * t1P * k * t4P + 4 * t4P * t6 * t14P * t3 * t1P * t7 * t3P * t2
         * t2 * t4 + 16 * t4P * t6 * t6 * t13P * t3 * t1P * t2P * t4 * t2 - 16 * t5 * t12kP * t2 * t2 * k * t1 * t4P * t7 * t3 + 6 * t5 * t12kP * t13P
         * t4P * t7 * t2 + 6 * t5 * t12kP * t13P * t4P * t6 * t3 + 6 * t5 * t5 * t12kP * t13P * t4P * t3 + 8 * t4P * t7 * t7 * t1P * t14P
         * t3P * t2 - 18 * t4P * t6 * t6 * t4 * t1P * k * t1 * t1 * t2P * t3 * t3 + 8 * t4P * t7 * t2 * t3 * t1P * t1 * t1 * t6 * t2P + 8 * t4P * t7
         * t7 * t2 * t3 * t1P * t1 * t1 * t3P + 2 * t6 * t2P * t4 * t4 * t1P * k * t7 * t3P * t2 + 6 * t5 * t12kP * k * t1 * t1 * t6 * t2P - 6 * t5 * t5
         * t12kP * k * t1 * t1 * t4P * t4 - 6 * t5 * t12kP * k * t1 * t1 * t4P * t6 * t4 + 4 * t4P * t4 * t5 * t1P * t1 * t1 * t7 * t3P * t3 + 4 * t4P
         * t4 * t5 * t1P * t1 * t1 * t6 * t2P * t3 * t3 + 6 * t5 * t12kP * t13P * t4P * t7 * t3 - 2 * t5 * t12kP * k * t4P * t6 * t2 * t2 * t3 * t3 - 8
         * t7 * t3P * t14P * t3 * t1P * t4P * t2 * t4 + 4 * t7 * t7 * t3P * t4 * t1P * t1 * t1 * t4P * t3 + t4P * t7 * t2 * t2 * t4 * t12kP
         * k * k * t5 - 16 * t42kP * t6 * t6 * t13P * t3 * t1P * t2 + 2 * t4P * t7 * t2 * t12kP * k * k * t5 + 16 * t42kP * t6 * t13P * t3 * t3 * t1P
         * t2 + 8 * t42kP * t7 * t2 * t2 * t1P * k * t6 * t13P - 4 * t7 * t7 * t32kP * t2 * t2 * t4 * t1P * t1 * t1 + 8 * t7 * t3P * t1P * k * t13P
         * t4P * t2 * t2 * t3 * t4 + 4 * t7 * t7 * t3P * t4 * t1P * t1 * t1 * t4P - 8 * t7 * t3P * t4 * t1P * t1 * t1 * t6 * t2P * t3 + 12 * t5 * t12kP
         * t3 * k * t1 * t1 * t6 * t2P - 6 * t5 * t12kP * t2 * t2 * t3 * t3 * k * t1 * t1 * t4P * t7 * t4 - 16 * t4P * t5 * t5 * t12kP * k * t3 * t1 - 18 * t6
         * t6 * t2P * t3 * t3 * t1P * k * t1 * t1 * t4P - 18 * t6 * t2P * t3 * t3 * t1P * k * t1 * t1 * t4P * t7 - 12 * t4P * t5 * t12kP * t2 * t3
         * t1 * t1 - 16 * t5 * t5 * t12kP * t2 * k * t1 * t4P - 8 * t7 * t3P * t13P * t3 * t1P * t6 * t2P * t2 + 4 * t7 * t7 * t3P * t14P * t3
         * t1P * t4P + 16 * t7 * t7 * t3P * t13P * t3 * t1P * t4P * t2 + 4 * t4P * t6 * t14P * t3 * t1P * t7 * t3P + 16 * t4P * t6
         * t13P * t3 * t1P * t7 * t3P * t2 - 16 * t5 * t5 * t12kP * t3 * t4 * k * t1 * t4P * t2 * t2 - 16 * t5 * t12kP * t3 * t4 * k * t1 * t4P * t7
         * t2 * t2 + 6 * t5 * t12kP * t3 * k * t1 * t1 * t7 * t3P + 6 * t4P * t7 * t5 * t12kP * t2 * t2 * t1 * t1 * t3 - 8 * t4P * t13P * t1P * t7
         * t3P * t2 * t2 * t4 - 18 * t4P * t6 * t6 * t2 * t3 * t3 * t1P * k * t1 * t1 * t2P - 12 * t4P * t6 * t6 * t2 * t3 * t3 * t1P * k * t1 * t2P
         * t4 + 4 * t5 * t12kP * k * t6 * t2P * t3 * t4 * t4 - 36 * t6 * t6 * t2P * t3 * t1P * k * t1 * t1 * t4P - 12 * t6 * t6 * t2P * t3 * t3 * t1P
         * k * t1 * t4P - 12 * t6 * t2P * t3 * t3 * t1P * k * t1 * t4P * t7 - 8 * t4P * t4 * t5 * t1P * k * t13P * t7 * t3P + 2 * t7 * t3P * t4
         * t5 * t1P * k * k * t1 * t1 * t4P * t2 * t2 + 4 * t7 * t3P * t4 * t5 * t1P * k * k * t1 * t4P * t2 * t2 * t3 + 4 * t4P * t2 * t5 * t1P * k * k
         * t6 * t2P * t1 * t3 * t3 * t4 + 4 * t4P * t2 * t5 * t1P * k * k * t6 * t2P * t3 * t1 * t1 + 2 * t4P * t2 * t5 * t1P * k * k * t6 * t2P * t4 * t1
         * t1 + 2 * t4P * t2 * t2 * t5 * t1P * k * k * t7 * t3P * t1 * t1 + 2 * t4P * t2 * t5 * t1P * k * k * t6 * t2P * t1 * t1 + 16 * t4P * t3 * t4 * t1P
         * k * t6 * t2P * t13P * t2 + 2 * t5 * t5 * t13kP * k * t2 * t2 * t3 * t3 * t1 + 2 * t5 * t12kP * k * t6 * t2P * t3 * t3 * t2 - 4 * t7 * t3P * t1P
         * k * k * t1 * t1 * t6 * t2P * t4 + 2 * t7 * t3P * t1P * k * k * t1 * t1 * t4P * t6 * t2 * t2 * t3 * t4 + 6 * t4P * t5 * t5 * t12kP * t2 * t4 * t1
         * t1 + 8 * t4P * t14P * t5 * t1P * t6 * t2P * t3 * t2 + 4 * t4P * t14P * t5 * t1P * t7 * t3P * t3 + 16 * t4P * t13P * t5 * t1P
         * t7 * t3P * t2 * t3 + 8 * t4P * t13P * t5 * t1P * t7 * t3P * t2 * t2 * t3 + 8 * t4P * t13P * t5 * t1P * t6 * t2P * t3 * t3 * t2 + 8
         * t4P * t13P * t5 * t1P * t6 * t2P * t3 * t3 + 8 * t4P * t13P * t5 * t1P * t7 * t3P * t3 + 2 * t7 * t3P * t1P * k * k * t1 * t1
         * t4P * t6 * t3 * t4 + 2 * t7 * t7 * t3P * t1P * k * k * t1 * t1 * t4P * t3 * t4 + 4 * t4P * t6 * t3 * t1P * t1 * t1 * t7 * t3P * t4 + 2 * t4P
         * t5 * t1P * k * k * t1 * t1 * t6 * t2P + 4 * t4P * t5 * t1P * k * k * t1 * t6 * t2P * t4 - 8 * t4P * t2 * t3 * t4 * t1P * k * k * t7 * t3P
         * t1 - 24 * t5 * t12kP * t2 * t3 * k * t1 * t1 * t4P * t6 * t4 + 8 * t4P * t6 * t13P * t3 * t1P * t7 * t3P * t4 + 3 * t4P * t7 * t5 * t12kP
         * t2 * t2 * t3 * t3 * t1 * t1 + 16 * t5 * t12kP * t3 * t4 * k * t1 * t7 * t3P * t2 * t2 + 4 * t7 * t3P * t4 * t5 * t1P * k * k * t1 * t4P * t2 * t2 - 12
         * t5 * t5 * t12kP * t2 * t3 * t3 * k * t1 * t1 * t4P * t4 - 4 * t7 * t3P * t1P * k * k * t1 * t1 * t4P * t2 - 2 * t7 * t3P * t1P * k * k * t1
         * t1 * t4P * t3 - 8 * t7 * t3P * t1P * k * k * t1 * t4P * t2 * t3 - 24 * t5 * t12kP * t2 * t3 * k * t1 * t1 * t4P * t7 * t4 + 12 * t4P * t5
         * t5 * t12kP * t2 * t4 * t1 * t1 * t3 + 3 * t4P * t5 * t5 * t12kP * t2 * t2 * t4 * t1 * t1 + 6 * t4P * t5 * t5 * t12kP * t2 * t2 * t4 * t1 * t1
         * t3 + 6 * t4P * t5 * t5 * t12kP * t2 * t4 * t13P + 12 * t4P * t5 * t5 * t12kP * t2 * t4 * t13P * t3 + 3 * t4P * t5 * t5 * t12kP * t2
         * t2 * t4 * t13P + 6 * t4P * t5 * t5 * t12kP * t2 * t2 * t4 * t13P * t3 + 4 * t4P * t5 * t1P * k * k * t1 * t6 * t2P - 16 * t5 * t12kP
         * t3 * t4 * k * t1 * t4P * t6 + 4 * t4P * t7 * t7 * t1P * t1 * t1 * t3P * t3 + 4 * t4P * t7 * t7 * t1P * t1 * t1 * t3P * t2 * t2 - 36 * t7 * t3P
         * t1P * k * t1 * t1 * t4P * t6 * t2 + 18 * t7 * t3P * t1P * k * t1 * t1 * t4P * t3 - 8 * t4P * t7 * t5 * t12kP * k * t2 * t2 * t3 * t3 * t1
         + 8 * t6 * t2P * t13P * t1P * t4P * t7 - 2 * t6 * t6 * t2P * t2 * t3 * t3 * t1P * k * t4P - 8 * t4P * t4 * t1P * t1 * t1 * t7 * t3P
         * t2 - 24 * t5 * t5 * t12kP * t2 * t3 * k * t1 * t1 * t4P * t4 + 2 * t4P * t6 * t6 * t1P * k * k * t2P * t2 + 4 * t4P * t6 * t6 * t1P * k * k
         * t2P * t3 * t2 + 4 * t4P * t6 * t6 * t1P * k * k * t2P * t1 * t2 - 4 * t7 * t3P * t14P * t3 * t1P * t4P * t4 - 8 * t7 * t3P * t13P
         * t3 * t1P * t4P - 4 * t7 * t3P * t14P * t3 * t1P * t6 * t2P * t2 - 8 * t6 * t2P * t1P * k * t13P * t4P * t7 * t4 + 12 * t5 * t12kP
         * t2 * t3 * k * t1 * t1 * t6 * t2P * t4 * t4 + 3 * t5 * t12kP * t13P * t4P * t6 * t3 * t3 - 3 * t5 * t12kP * t13P * t4P * t2 * t2 - 8 * t4P
         * t5 * t5 * t12kP * k * t2 * t2 * t4 * t1 - 12 * t4P * t5 * t5 * t12kP * k * t2 * t4 * t1 * t1 - 6 * t4P * t5 * t5 * t12kP * k * t2 * t2 * t4 * t1
         * t1 + 8 * t5 * t12kP * t2 * t4 * k * t7 * t3P * t3 + 4 * t5 * t12kP * t2 * t4 * k * t6 * t2P * t3 * t3 + 8 * t6 * t6 * t2P * t13P * t1P * t4P
         + 8 * t7 * t3P * t5 * t12kP * k * t1 + 16 * t7 * t3P * t5 * t12kP * k * t1 * t2 + 12 * t7 * t3P * t5 * t12kP * k * t1 * t1 * t2 + 8 * t7 * t7
         * t3P * t14P * t3 * t1P * t4P * t2 - 4 * t7 * t3P * t1P * k * k * t1 * t4P * t2 * t2 - 16 * t4P * t4 * t5 * t1P * k * t13P * t7
         * t3P * t2 * t3 + 2 * t4P * t3 * t3 * t4 * t1P * k * t6 * t2P * t2 + 24 * t4P * t3 * t4 * t1P * k * t6 * t2P * t1 * t2 - 2 * t4P * t2 * t2
         * t1P * k * k * t7 * t3P - 4 * t4P * t2 * t1P * k * k * t7 * t3P * t4 - 2 * t7 * t3P * t1P * k * k * t1 * t1 * t4P - 8 * t7 * t3P * t1P
         * k * k * t1 * t4P * t2 - 8 * t4P * t5 * t1P * k * t13P * t6 * t2P + 8 * t7 * t3P * t1P * k * k * t1 * t4P * t6 * t2 * t3 - 4 * t7 * t3P
         * t1P * k * k * t1 * t1 * t4P * t2 * t3 - 36 * t4P * t4 * t5 * t1P * k * t1 * t1 * t6 * t2P * t3 * t2 - 18 * t4P * t4 * t5 * t1P * k * t1 * t1
         * t7 * t3P * t3 - 24 * t4P * t4 * t5 * t1P * k * t1 * t7 * t3P * t2 * t3 - 8 * t4P * t4 * t5 * t1P * k * t13P * t6 * t2P * t3 * t3 - 8 * t4P
         * t4 * t5 * t1P * k * t13P * t7 * t3P * t3 + 2 * t7 * t3P * t4 * t5 * t1P * k * k * t1 * t1 * t4P * t2 * t2 * t3 - 12 * t4P * t4 * t5 * t1P
         * k * t1 * t6 * t2P * t2 + 4 * t5 * t5 * t13kP * k * t2 * t2 * t3 * t3 * t1 * t4 - 12 * t4P * t7 * t2 * t1P * k * t6 * t2P * t1 * t4 + 2 * t5 * t12kP
         * k * t6 * t2P * t4 * t4 - 4 * t5 * t5 * t12kP * k * t4P * t2 * t3 * t3 - 12 * t4P * t7 * t7 * t2 * t2 * t1P * k * t3P * t1 * t4 * t3 - 4 * t4P
         * t4 * t1P * t1 * t1 * t7 * t3P * t2 * t2 * t3 - 16 * t4P * t7 * t5 * t12kP * k * t2 * t4 * t1 - 8 * t5 * t12kP * k * t4P * t7 * t2 * t3 + 12
         * t5 * t12kP * t2 * t3 * t3 * k * t1 * t1 * t4P * t4 + 12 * t5 * t12kP * t2 * t3 * k * t1 * t1 * t7 * t3P * t4 * t4 - 6 * t4P * t5 * t12kP * t3
         * t1 * t1 - 4 * t7 * t3P * t1P * k * k * t1 * t4P * t2 * t2 * t3 - 2 * t7 * t3P * t1P * k * k * t1 * t1 * t4P * t2 * t2 + 4 * t7 * t3P * t1P
         * k * k * t1 * t4P * t6 * t2 * t2 - 2 * t5 * t12kP * k * t4P * t6 * t4 + 32 * t7 * t3P * t5 * t12kP * k * t1 * t2 * t4 + 6 * t7 * t3P * t5 * t12kP
         * k * t1 * t1 + 12 * t7 * t3P * t5 * t12kP * k * t1 * t1 * t4 + 24 * t7 * t3P * t5 * t12kP * k * t1 * t1 * t2 * t4 - 16 * t6 * t2P * t3 * t1P
         * k * t13P * t4P * t7 * t4 - 4 * t7 * t3P * t14P * t3 * t1P * t6 * t2P * t4 * t4 + 8 * t4P * t2 * t2 * t4 * t5 * t1P * t13P * t7 * t3P
         * t3 - 6 * t7 * t3P * t5 * t12kP * t2 * t2 * t1 * t1 * t4 - 16 * t5 * t5 * t12kP * t3 * t4 * k * t1 * t4P + 4 * t7 * t3P * t1P * k * k * t1 * t4P
         * t6 * t3 + 4 * t7 * t3P * t1P * k * k * t1 * t4P * t5 * t3 + 2 * t5 * t5 * t13kP * k * t2 * t2 * t3 * t3 * t4 * t4 * t1 - 2 * t5 * t12kP * k * t4P
         * t7 * t3 * t3 * t4 + 3 * t5 * t12kP * t13P * t4P * t7 * t3 * t3 + 6 * t5 * t12kP * t13P * t4P * t7 * t2 * t3 * t3 - 24 * t4P * t7 * t7
         * t2 * t1P * k * t3P * t1 * t4 + 6 * t5 * t12kP * t13P * t4P * t6 * t2 * t3 * t3 + 2 * t7 * t7 * t3P * t1P * k * k * t1 * t1 * t4P + 2 * t7
         * t3P * t1P * k * k * t1 * t1 * t4P * t6 - 12 * t4P * t4 * t5 * t1P * k * t1 * t7 * t3P * t2 * t2 * t3 - 12 * t4P * t4 * t5 * t1P * k * t1
         * t6 * t2P * t3 * t3 * t2 - 12 * t4P * t4 * t5 * t1P * k * t1 * t6 * t2P * t3 * t3 - 12 * t4P * t4 * t5 * t1P * k * t1 * t7 * t3P * t3 - 4 * t7
         * t3P * t1P * k * k * t1 * t6 * t2P * t2 - 8 * t4P * t4 * t1P * t1 * t1 * t7 * t3P * t2 * t3 - 4 * t4P * t4 * t1P * t1 * t1 * t7 * t3P
         * t3 - 4 * t4P * t4 * t1P * t1 * t1 * t6 * t2P * t3 * t3 - 4 * t4P * t4 * t1P * t1 * t1 * t7 * t3P * t2 * t2 - 4 * t4P * t4 * t1P * t1 * t1
         * t6 * t2P * t2 - 12 * t4P * t7 * t7 * t2 * t2 * t1P * k * t3P * t1 - 8 * t4P * t4 * t5 * t1P * k * t13P * t7 * t3P * t2 * t2 * t3 + 4 * t7
         * t7 * t3P * t4 * t1P * t14P * t4P - 4 * t7 * t3P * t1P * k * k * t1 * t4P - 2 * t7 * t3P * t1P * k * k * t1 * t1 * t6 * t2P * t2 - 2
         * t7 * t3P * t1P * k * k * t1 * t1 * t4P * t4 + 8 * t4P * t7 * t7 * t14P * t2 * t4 * t1P * t3P + 8 * t4P * t7 * t7 * t13P * t2 * t2
         * t4 * t1P * t3P - 12 * t5 * t12kP * t1 * t1 * t7 * t3P * t2 * t4 * t3 - 6 * t5 * t12kP * t1 * t1 * t6 * t2P * t3 * t3 * t4 + 3 * t5 * t12kP
         * t1 * t1 * t4P * t7 * t3 * t3 + 6 * t5 * t12kP * t1 * t1 * t4P * t7 * t2 * t3 * t3 - 3 * t5 * t12kP * t1 * t1 * t4P * t2 * t2 - 12 * t5 * t12kP
         * t1 * t1 * t6 * t2P * t3 * t4 - 2 * t4P * t6 * t6 * t1P * k * t2P - 24 * t4P * t7 * t2 * t1P * k * t6 * t2P * t1 * t3 - 4 * t4P * t7 * t2
         * t1P * k * t6 * t2P * t3 * t4 + 8 * t4P * t1P * k * t13P * t6 * t2P * t3 * t3 + 18 * t4P * t1P * k * t1 * t1 * t6 * t2P * t3 * t3 + 2
         * t6 * t6 * t2P * t3 * t3 * t4 * t1P * k * k * t4P + 2 * t6 * t2P * t3 * t3 * t4 * t1P * k * k * t4P * t5 + 12 * t7 * t3P * t2 * t1P * k * t1
         * t6 * t2P * t3 + 24 * t7 * t3P * t2 * t1P * k * t1 * t6 * t2P * t4 - 6 * t5 * t12kP * t3 * t3 * k * t1 * t1 * t4P * t6 + 2 * t7 * t3P * t1P
         * k * k * t1 * t1 * t4P * t5 + 4 * t7 * t7 * t3P * t1P * k * k * t1 * t4P * t3 + 8 * t4P * t4 * t5 * t1P * t1 * t1 * t6 * t2P * t3 - 2 * t5 * t12kP
         * k * t4P * t7 * t4 + 4 * t5 * t12kP * k * t7 * t3P * t4 * t3 + 6 * t5 * t12kP * t1 * t1 * t4P * t7 * t2 + 6 * t5 * t12kP * t1 * t1 * t4P
         * t6 * t3 + 6 * t5 * t5 * t12kP * t1 * t1 * t4P * t3 + 6 * t5 * t12kP * t1 * t1 * t4P * t7 * t3 + 3 * t5 * t12kP * t1 * t1 * t4P * t7 * t2 * t2
         * t4 - 3 * t5 * t12kP * t1 * t1 * t6 * t2P * t3 * t3 * t4 * t4 * t2 - 6 * t5 * t12kP * t1 * t1 * t6 * t2P * t3 * t2 + 6 * t5 * t5 * t12kP * t1 * t1
         * t4P * t2 * t3 * t3 + 8 * t5 * t12kP * t3 * t3 * t4 * k * t1 * t4P * t2 * t2 - 36 * t4P * t6 * t4 * t1P * k * t1 * t1 * t7 * t3P * t2 + 6 * t5
         * t12kP * t2 * t2 * t3 * k * t1 * t1 * t7 * t3P * t4 * t4 - 6 * t5 * t12kP * t2 * t2 * t3 * t3 * k * t1 * t1 * t4P * t6 * t4 - 12 * t5 * t12kP * t2
         * t2 * t3 * k * t1 * t1 * t4P * t6 * t4 + 6 * t5 * t12kP * t13P * t4P * t7 * t2 * t2 * t3 - 12 * t5 * t12kP * t2 * t2 * t3 * k * t1 * t1 * t4P
         * t7 * t4 + 2 * t6 * t2P * t3 * t1P * k * t7 * t3P + 2 * t6 * t2P * t3 * t3 * t1P * k * t4P - 6 * t5 * t12kP * t3 * t3 * k * t1 * t1 * t4P
         * t7 - 8 * t5 * t12kP * k * t1 * t4P * t7 * t3 * t3 + 4 * t7 * t7 * t3P * t1P * k * k * t1 * t4P * t4 - 8 * t4P * t6 * t4 * t1P * k * t13P
         * t7 * t3P + 2 * t6 * t2P * t3 * t3 * t4 * t1P * k * k * t4P * t7 - 4 * t6 * t2P * t3 * t4 * t1P * k * k * t4P - 12 * t5 * t12kP * t2 * t3
         * t3 * k * t1 * t1 * t4P * t6 * t4 - 12 * t5 * t5 * t12kP * t2 * t2 * t3 * k * t1 * t1 * t4P * t4 - 2 * t4P * t6 * t1P * k * t7 * t3P - 4 * t7 * t3P
         * t1P * k * k * t1 * t4P * t4 - 12 * t5 * t12kP * t2 * t4 * k * t4P * t6 * t1 * t1 - 6 * t6 * t2P * t5 * t12kP * t2 * t4 * t1 * t1 - 4 * t5 * t12kP
         * t2 * t4 * k * t4P * t6 - 4 * t5 * t5 * t12kP * t2 * t4 * k * t4P - 6 * t5 * t12kP * t2 * t2 * t4 * k * t4P * t6 * t1 * t1 - 2 * t7 * t3P * t1P
         * k * k * t1 * t1 * t6 * t2P * t4 * t4 - 2 * t7 * t3P * t1P * k * k * t4P * t3 * t4 + 2 * t7 * t3P * t1P * k * k * t1 * t1 * t4P * t5 * t3 + 4 * t4P
         * t6 * t6 * t2 * t1P * t1 * t1 * t2P - 8 * t4P * t4 * t1P * t1 * t1 * t6 * t2P * t3 * t2 - 4 * t6 * t2P * t2 * t1P * t1 * t1 * t7 * t3P - 8
         * t6 * t2P * t2 * t1P * t1 * t1 * t4P * t3 + 18 * t4P * t3 * t3 * t4 * t1P * k * t6 * t2P * t1 * t1 + 8 * t6 * t6 * t2P * t3 * t1P * k * k
         * t1 * t4P + 8 * t4P * t6 * t6 * t13P * t3 * t3 * t1P * t2P * t2 - 12 * t6 * t6 * t2P * t2 * t1P * k * t1 * t4P * t4 - 8 * t4P * t7
         * t5 * t12kP * k * t2 * t2 * t4 * t1 - 12 * t4P * t7 * t5 * t12kP * k * t2 * t4 * t1 * t1 + 8 * t4P * t7 * t1P * t1 * t1 * t6 * t2P * t3 + 4 * t4P
         * t7 * t1P * t1 * t1 * t6 * t2P * t4 + 4 * t4P * t7 * t1P * t1 * t1 * t6 * t2P + 4 * t4P * t7 * t7 * t1P * t1 * t1 * t3P - 4 * t6 * t2P
         * t3 * t1P * k * t4P * t7 - 4 * t6 * t6 * t2P * t3 * t1P * k * t4P + 16 * t4P * t2 * t4 * t5 * t1P * t13P * t7 * t3P * t3 + 4 * t4P
         * t2 * t2 * t4 * t5 * t1P * t1 * t1 * t7 * t3P * t3 + 4 * t4P * t2 * t4 * t5 * t1P * t14P * t6 * t2P * t3 * t3 + 8 * t4P * t2 * t4 * t5 * t1P
         * t14P * t7 * t3P * t3 + 4 * t4P * t2 * t2 * t4 * t5 * t1P * t14P * t7 * t3P * t3 - 4 * t5 * t12kP * t2 * t4 * k * t4P * t7 * t3 * t3 - 2
         * t5 * t12kP * t2 * t2 * t4 * k * t4P * t7 * t3 * t3 - 8 * t4P * t6 * t6 * t4 * t1P * k * t13P * t2P * t3 * t3 * t2 + 8 * t6 * t2P * t3 * t1P
         * k * k * t1 * t4P * t5 + 2 * t5 * t12kP * k * t4P * t3 * t3 + 16 * t4P * t7 * t7 * t13P * t2 * t4 * t1P * t3P * t3 + 8 * t4P * t7 * t13P
         * t2 * t4 * t1P * t6 * t2P * t3 * t3 + 12 * t5 * t12kP * t2 * t2 * t3 * k * t1 * t1 * t7 * t3P * t4 + 8 * t7 * t3P * t1P * k * t13P * t4P
         * t4 + 18 * t7 * t3P * t1P * k * t1 * t1 * t4P + 18 * t7 * t3P * t1P * k * t1 * t1 * t6 * t2P * t3 * t2 - 12 * t4P * t7 * t7 * t2 * t2 * t1P
         * k * t3P * t1 * t3 - 4 * t6 * t2P * t14P * t1P * t4P * t2 * t3 * t3 + 6 * t5 * t12kP * k * t1 * t1 * t4P * t4 + 36 * t4P * t3 * t4 * t1P
         * k * t6 * t2P * t1 * t1 + 16 * t4P * t3 * t4 * t1P * k * t6 * t2P * t13P + 2 * t7 * t3P * t1P * k * k * t1 * t1 * t4P * t6 * t2 * t2 * t3
         + 4 * t7 * t3P * t2 * t3 * t4 * t1P * k * k * t4P * t6 + 2 * t7 * t3P * t1P * k * k * t4P * t6 * t3 * t4 + 8 * t4P * t6 * t6 * t14P * t3 * t1P
         * t2P * t4 * t2 - 8 * t6 * t2P * t13P * t1P * t4P * t2 * t4 - 4 * t5 * t12kP * k * t4P * t7 * t2 * t3 * t3 + 16 * t7 * t3P * t5 * t12kP
         * k * t2 * t4 * t4 * t1 - t5 * t5 * t13kP * t3 * t3 * t4 * t4 * t1 * t1 - 2 * t4P * t4 * t5 * t1P * k * t7 * t3P + 12 * t7 * t3P * t5 * t12kP
         * k * t2 * t4 * t4 * t1 * t1 - 2 * t4P * t4 * t5 * t1P * k * t6 * t2P + 2 * t4P * t7 * t7 * t2 * t2 * t1P * k * k * t1 * t1 * t3P * t4 * t3 + 2 * t6
         * t2P * t3 * t3 * t1P * k * k * t1 * t1 * t4P * t7 * t4 - 12 * t4P * t7 * t2 * t1P * k * t6 * t2P * t1 + 16 * t7 * t3P * t1P * k * t13P
         * t6 * t2P * t3 * t4 - 8 * t7 * t7 * t3P * t1P * k * t13P * t4P * t3 - 4 * t4P * t4 * t5 * t1P * k * t6 * t2P * t3 - 16 * t4P * t7 * t2
         * t1P * k * t6 * t2P * t13P * t3 - 2 * t7 * t3P * t1P * k * k * t4P * t4 + 2 * t6 * t6 * t2P * t3 * t3 * t1P * k * k * t1 * t1 * t4P * t4
         + 6 * t7 * t3P * t5 * t12kP * k * t2 * t2 * t4 * t4 * t1 * t1 + 4 * t6 * t2P * t3 * t1P * k * k * t1 * t1 * t4P * t7 * t2 - 16 * t4P * t7 * t7 * t2
         * t1P * k * t3P * t13P - 2 * t4P * t4 * t5 * t1P * k * t6 * t2P * t2 - 2 * t4P * t4 * t5 * t1P * k * t7 * t3P * t2 * t2 - 2 * t4P
         * t4 * t5 * t1P * k * t6 * t2P * t3 * t3 - 4 * t4P * t7 * t2 * t1P * k * t6 * t2P * t3 - 2 * t4P * t7 * t2 * t1P * k * t6 * t2P * t4 - 18
         * t4P * t6 * t2 * t2 * t3 * t1P * k * t1 * t1 * t7 * t3P - 12 * t4P * t6 * t2 * t2 * t3 * t1P * k * t1 * t7 * t3P * t4 - 2 * t6 * t2P * t3 * t1P
         * k * k * t1 * t1 * t7 * t3P * t4 * t4 - 2 * t4P * t4 * t5 * t1P * k * t7 * t3P * t3 - 4 * t5 * t12kP * k * t4P * t7 * t3 * t4 + 4 * t6 * t6 * t2P
         * t3 * t3 * t1P * k * k * t1 * t4P * t2 + 12 * t5 * t12kP * t3 * t3 * t4 * k * t1 * t1 * t6 * t2P - 3 * t4P * t5 * t12kP * t3 * t3 * t13P - 4
         * t4P * t4 * t5 * t1P * k * t7 * t3P * t2 + 12 * t5 * t12kP * t13P * t4P * t7 * t2 * t3 + 16 * t5 * t12kP * t3 * t4 * k * t1 * t4P * t2
         * t2 - 2 * t6 * t2P * t2 * t4 * t4 * t1P * k * k * t7 * t3P + 12 * t5 * t12kP * t3 * t4 * t4 * k * t1 * t1 * t6 * t2P + 4 * t7 * t7 * t3P * t2 * t4
         * t1P * k * k * t4P - 24 * t5 * t12kP * t2 * t3 * k * t1 * t1 * t4P * t7 - 4 * t6 * t2P * t14P * t1P * t4P * t3 * t3 * t4 + 8 * t6 * t2P
         * t13P * t1P * t4P * t7 * t3 * t3 * t4 - 4 * t6 * t2P * t14P * t1P * t7 * t3P * t2 * t4 * t4 + 8 * t6 * t6 * t2P * t13P * t1P
         * t4P * t4 - 2 * t5 * t12kP * k * t4P * t7 * t2 * t2 * t3 * t3 + 12 * t4P * t3 * t3 * t4 * t1P * k * t6 * t2P * t1 + 2 * t6 * t2P * t3 * t3
         * t1P * k * k * t1 * t1 * t4P * t7 * t2 * t4 - 8 * t6 * t2P * t3 * t1P * k * k * t1 * t7 * t3P * t2 * t4 - 2 * t6 * t2P * t3 * t1P * k * k * t1
         * t1 * t7 * t3P * t2 - 4 * t6 * t2P * t3 * t1P * k * k * t1 * t1 * t7 * t3P * t4 - 2 * t6 * t2P * t3 * t3 * t1P * k * k * t1 * t1 * t4P * t2 + 4
         * t6 * t6 * t2P * t3 * t1P * k * k * t1 * t1 * t4P * t2 - 4 * t4P * t7 * t7 * t2 * t1P * k * t3P - 36 * t4P * t7 * t7 * t2 * t1P * k * t3P
         * t1 * t1 - 18 * t4P * t6 * t4 * t1P * k * t1 * t1 * t7 * t3P * t2 * t2 * t3 + 2 * t6 * t6 * t2P * t3 * t3 * t1P * k * k * t1 * t1 * t4P * t2 + 2
         * t4P * t7 * t2 * t1P * k * k * t1 * t1 * t6 * t2P * t4 + 12 * t7 * t3P * t2 * t1P * k * t1 * t6 * t2P * t3 * t4 * t4 + 8 * t5 * t12kP * t3
         * t3 * t4 * t4 * k * t1 * t6 * t2P - 16 * t5 * t12kP * t3 * t3 * t4 * k * t1 * t4P * t6 * t2 + 2 * t7 * t3P * t2 * t2 * t3 * t1P * k * k * t4P * t6 - 2
         * t4P * t7 * t2 * t1P * k * t6 * t2P + 4 * t6 * t2P * t3 * t3 * t1P * k * k * t1 * t4P * t7 * t2 + 6 * t5 * t5 * t12kP * t13P * t4P
         * t2 * t3 * t3 + 3 * t4P * t7 * t5 * t12kP * t2 * t2 * t3 * t3 * t4 * t1 * t1 + 12 * t4P * t4 * t1P * k * t6 * t2P * t1 + 4 * t6 * t2P * t3 * t4
         * t1P * k * k * t4P * t7 - 2 * t5 * t12kP * k * t4P * t6 * t3 * t3 * t4 - 8 * t6 * t6 * t2P * t2 * t1P * k * t13P * t4P + 3 * t4P * t7
         * t5 * t12kP * t2 * t2 * t3 * t3 * t4 * t13P + 24 * t7 * t3P * t2 * t1P * k * t1 * t6 * t2P * t3 * t4 + 2 * t6 * t2P * t2 * t4 * t1P * k * k
         * t4P * t7 * t3 * t3 - 16 * t4P * t6 * t4 * t1P * k * t13P * t7 * t3P * t2 * t3 - 2 * t4P * t2 * t1P * k * k * t6 * t2P * t1 * t1 - 6 * t5
         * t12kP * t3 * t3 * t4 * k * t1 * t1 * t4P * t6 - 6 * t5 * t12kP * t3 * t3 * t4 * k * t1 * t1 * t4P * t7 - 6 * t4P * t5 * t12kP * t3 * t13P
         + 32 * t5 * t12kP * t3 * t4 * k * t1 * t7 * t3P * t2 + 8 * t5 * t12kP * t3 * t4 * t4 * k * t1 * t7 * t3P + 6 * t5 * t5 * t12kP * t3 * t4 * t1 * t1
         * t4P + 4 * t4P * t6 * t6 * t1P * t1 * t1 * t2P * t4 - 8 * t4P * t7 * t2 * t1P * k * t6 * t2P * t13P * t3 * t3 - 3 * t5 * t12kP * t13P
         * t4 * t4P * t3 * t3 + 6 * t5 * t12kP * t13P * t4 * t4P * t6 * t3 + 6 * t5 * t12kP * t13P * t4 * t4P * t7 * t3 - 16 * t6 * t2P * t13P
         * t1P * t4P * t3 * t4 - 4 * t5 * t12kP * k * t4P * t6 * t3 * t4 - 16 * t6 * t2P * t3 * t1P * k * t13P * t4P * t7 - 16 * t6 * t6 * t2P
         * t3 * t1P * k * t13P * t4P - 8 * t4P * t6 * t6 * t4 * t1P * k * t13P * t2P * t3 * t3 + 16 * t4P * t13P * t5 * t1P * t7 * t3P
         * t2 + 8 * t4P * t13P * t5 * t1P * t7 * t3P * t4 + 2 * t6 * t2P * t3 * t3 * t1P * k * k * t1 * t1 * t4P * t7 + 8 * t7 * t3P * t1P * k
         * t13P * t4P * t2 * t2 * t3 - 36 * t4P * t6 * t4 * t1P * k * t1 * t1 * t7 * t3P * t2 * t3 - 12 * t6 * t2P * t1P * k * t1 * t4P * t7 * t4 - 24
         * t5 * t12kP * t2 * t3 * k * t1 * t1 * t4P * t6 - 8 * t4P * t6 * t4 * t1P * k * t13P * t7 * t3P * t2 * t2 * t3 + 2 * t6 * t6 * t2P * t3 * t3
         * t1P * k * k * t1 * t1 * t4P - 8 * t4P * t7 * t7 * t2 * t2 * t1P * k * t3P * t13P * t3 + 32 * t5 * t12kP * t3 * t4 * k * t1 * t6 * t2P - 4
         * t6 * t2P * t3 * t1P * k * k * t1 * t1 * t7 * t3P * t2 * t4 + 4 * t7 * t3P * t1P * k * k * t1 * t4P * t6 * t4 + 4 * t7 * t3P * t1P * k * k
         * t1 * t4P * t5 * t3 * t4 - 8 * t7 * t3P * t1P * k * t13P * t4P * t6 * t3 + 36 * t7 * t3P * t1P * k * t1 * t1 * t4P * t2 * t3 * t4 - 8 * t6
         * t2P * t3 * t3 * t5 * t1P * k * t13P * t4P * t2 + 6 * t5 * t12kP * t13P * t4P * t6 * t2 * t2 * t3 - 18 * t4P * t6 * t4 * t1P * k
         * t1 * t1 * t7 * t3P * t2 * t2 - 4 * t7 * t3P * t1P * k * k * t1 * t6 * t2P * t4 * t4 + 4 * t7 * t3P * t1P * k * k * t1 * t4P * t5 * t4 - 4 * t7
         * t3P * t1P * k * k * t1 * t1 * t4P * t2 * t4 - 12 * t4P * t6 * t1P * k * t1 * t7 * t3P * t4 - 18 * t7 * t7 * t3P * t1P * k * t1 * t1 * t4P
         * t4 - 4 * t6 * t2P * t2 * t4 * t1P * k * k * t4P * t1 + 8 * t7 * t3P * t1P * k * t13P * t4P * t3 * t4 + 3 * t5 * t5 * t12kP * t3 * t3 * t4
         * t1 * t1 * t4P + 3 * t5 * t5 * t12kP * t3 * t3 * t4 * t13P * t4P + 6 * t5 * t5 * t12kP * t3 * t3 * t4 * t1 * t1 * t4P * t2 + 16 * t5 * t12kP
         * t3 * t4 * k * t1 * t7 * t3P + 6 * t5 * t5 * t12kP * t3 * t4 * t13P * t4P + 4 * t4P * t7 * t7 * t2 * t1P * k * k * t1 * t1 * t3P * t4 + 2 * t4P
         * t7 * t7 * t2 * t2 * t1P * k * k * t1 * t1 * t3P - 24 * t4P * t7 * t7 * t2 * t1P * k * t3P * t1 - 2 * t4P * t7 * t7 * t2 * t2 * t1P * k * t3P - 4
         * t4P * t7 * t7 * t2 * t1P * k * t3P * t4 + 2 * t5 * t12kP * k * t7 * t3P * t4 * t4 * t3 + 8 * t7 * t3P * t1P * k * t13P * t6 * t2P
         * t3 * t2 + 18 * t7 * t3P * t1P * k * t1 * t1 * t6 * t2P * t4 * t4 * t2 + 8 * t7 * t3P * t1P * k * t13P * t4P * t2 * t2 * t4 + 18 * t7 * t3P
         * t1P * k * t1 * t1 * t6 * t2P * t4 * t4 + 16 * t7 * t3P * t1P * k * t13P * t4P * t2 * t4 - 36 * t4P * t6 * t6 * t2 * t3 * t1P * k * t1
         * t1 * t2P - 12 * t4P * t6 * t6 * t2 * t3 * t3 * t1P * k * t1 * t2P - 24 * t4P * t6 * t6 * t2 * t3 * t1P * k * t1 * t2P * t4 + 8 * t6 * t2P
         * t13P * t1P * t4P * t7 * t4 - 4 * t6 * t2P * t14P * t1P * t4P * t2 * t4 + 4 * t6 * t2P * t14P * t1P * t4P * t7 * t4 - 8 * t4P
         * t6 * t5 * t12kP * k * t3 * t3 * t1 + 2 * t4P * t2 * t5 * t1P * k * k * t6 * t2P * t4 + 6 * t5 * t12kP * t3 * t4 * t4 * k * t1 * t1 * t7 * t3P
         + 2 * t6 * t2P * t3 * t3 * t1P * k * k * t1 * t1 * t4P * t7 * t2 - 4 * t7 * t3P * t1P * k * k * t1 * t6 * t2P * t4 * t4 * t2 - 8 * t5 * t12kP
         * t3 * t3 * t4 * k * t1 * t4P * t7 * t2 * t2 + 4 * t4P * t7 * t7 * t2 * t2 * t1P * k * k * t1 * t3P * t4 + 4 * t6 * t2P * t4 * t1P * k * k * t1 * t4P
         * t7 - 24 * t4P * t7 * t7 * t2 * t1P * k * t3P * t1 * t3 - 4 * t4P * t7 * t7 * t2 * t1P * k * t3P * t4 * t3 - 2 * t4P * t7 * t7 * t2 * t2 * t1P
         * k * t3P * t3 - 18 * t7 * t7 * t3P * t1P * k * t1 * t1 * t4P * t3 * t4 - 6 * t5 * t12kP * k * t1 * t1 * t4P * t7 * t2 * t2 + 16 * t5 * t12kP
         * t3 * t4 * t4 * k * t1 * t7 * t3P * t2 - 2 * t6 * t2P * t2 * t4 * t4 * t1P * k * k * t7 * t3P * t3 - 4 * t6 * t2P * t3 * t3 * t1P * k * k * t1 * t4P
         * t2 * t4 + 2 * t6 * t2P * t3 * t3 * t1P * k * k * t1 * t1 * t4P * t5 * t4 - 4 * t7 * t3P * t14P * t3 * t1P * t6 * t2P - 4 * t7 * t3P * t14P
         * t3 * t1P * t6 * t2P * t4 * t4 * t2 + 8 * t7 * t3P * t5 * t12kP * k * t4 * t4 * t1 + 2 * t6 * t2P * t3 * t3 * t1P * k * k * t1 * t1 * t4P * t5 - 2
         * t6 * t2P * t3 * t3 * t1P * k * k * t1 * t1 * t4P * t4 - 18 * t4P * t4 * t5 * t1P * k * t1 * t1 * t6 * t2P * t3 * t3 - 2 * t7 * t3P * t1P
         * k * k * t1 * t1 * t4P * t2 * t2 * t4 + 2 * t7 * t7 * t3P * t1P * k * k * t1 * t1 * t4P * t4 + 2 * t42kP * t2 * t2 * t1P * k * k * t6 * t3 * t3 - 2
         * t7 * t7 * t32kP * t1P * k * k * t1 * t2 * t2 + 2 * t42kP * t2 * t2 * t3 * t1P * k - 8 * t42kP * t2 * t5 * t5 * t1P * t1 * t1 * t3 + 18 * t7
         * t3P * t1P * k * t1 * t1 * t4P * t4 + 4 * t4P * t5 * t1P * k * k * t6 * t2P * t3 + 2 * t4P * t5 * t1P * k * k * t6 * t2P * t4 + 2 * t4P
         * t5 * t1P * k * k * t6 * t2P + 2 * t6 * t2P * t2 * t1P * k * t4P - 2 * t7 * t3P * t2 * t3 * t1P * k * k * t6 * t2P + 4 * t4P * t3 * t4
         * t1P * k * t6 * t2P + 2 * t4P * t3 * t4 * t1P * k * t7 * t3P - 8 * t7 * t3P * t13P * t3 * t1P * t6 * t2P * t4 * t4 * t2 - 4 * t7 * t3P
         * t14P * t3 * t1P * t4P * t2 * t2 * t4 + 4 * t7 * t7 * t3P * t14P * t3 * t1P * t4P * t4 - 2 * t4P * t4 * t1P * k * k * t6 * t2P - 2
         * t7 * t7 * t3P * t3 * t1P * k * t4P - 8 * t6 * t2P * t14P * t1P * t7 * t3P * t2 * t4 + 16 * t4P * t1P * k * t13P * t6 * t2P
         * t3 - 16 * t5 * t12kP * t2 * k * t1 * t4P * t7 - 4 * t6 * t2P * t3 * t1P * k * t4P * t5 - 12 * t4P * t6 * t1P * k * t1 * t7 * t3P + 8 * t7
         * t7 * t3P * t2 * t4 * t1P * t1 * t1 * t4P - 24 * t6 * t2P * t3 * t1P * k * t1 * t4P * t7 + 8 * t6 * t2P * t14P * t1P * t4P * t7
         * t3 + 8 * t6 * t2P * t14P * t1P * t4P * t7 * t2 * t3 - 16 * t6 * t2P * t13P * t1P * t4P * t2 * t3 * t4 + 2 * t5 * t5 * t13kP * k
         * t3 * t3 * t4 * t4 * t1 - 2 * t6 * t2P * t1P * k * k * t4P * t3 * t3 + 6 * t4P * t7 * t5 * t12kP * t4 * t1 * t1 * t3 + 6 * t4P * t7 * t5 * t12kP
         * t4 * t1 * t1 * t2 + 12 * t4P * t7 * t5 * t12kP * t4 * t1 * t1 * t2 * t3 - 6 * t5 * t12kP * t13P * t6 * t2P * t3 * t2 + 8 * t7 * t3P * t1P
         * k * t13P * t6 * t2P * t4 * t4 - 16 * t7 * t3P * t1P * k * t13P * t4P * t6 * t2 + 4 * t5 * t5 * t13kP * k * t1 * t4 - 2 * t4P * t7 * t7
         * t4 * t1P * k * t3P - 2 * t4P * t7 * t7 * t3 * t4 * t1P * k * t3P + 4 * t5 * t5 * t13kP * k * t2 * t4 * t4 * t1 + 2 * t5 * t5 * t13kP * k * t2
         * t2 * t4 * t4 * t1 - 24 * t4P * t6 * t6 * t3 * t4 * t1P * k * t1 * t2P - 12 * t4P * t6 * t3 * t4 * t1P * k * t1 * t7 * t3P - 12 * t4P * t6
         * t6 * t3 * t3 * t4 * t1P * k * t1 * t2P - 12 * t5 * t12kP * k * t1 * t1 * t4P * t6 * t2 + 8 * t4P * t7 * t7 * t2 * t1P * k * k * t1 * t3P * t3
         + 4 * t4P * t7 * t7 * t2 * t1P * k * k * t1 * t1 * t3P * t3 + 4 * t4P * t7 * t7 * t2 * t2 * t1P * k * k * t1 * t3P * t3 + 8 * t4P * t7 * t7 * t2
         * t1P * k * k * t1 * t3P * t4 * t3 + 4 * t4P * t7 * t7 * t2 * t2 * t1P * k * k * t1 * t3P + 8 * t4P * t7 * t7 * t2 * t1P * k * k * t1 * t3P
         * t4 + 4 * t4P * t7 * t7 * t2 * t1P * k * k * t1 * t1 * t3P + 8 * t4P * t7 * t7 * t2 * t1P * k * k * t1 * t3P + 4 * t4P * t7 * t2 * t1P * k
         * k * t1 * t6 * t2P - 2 * t7 * t3P * t1P * k * k * t1 * t1 * t4P * t2 * t2 * t3 + 4 * t4P * t7 * t2 * t1P * k * k * t1 * t6 * t2P * t4 + 2 * t4P
         * t7 * t2 * t1P * k * k * t1 * t1 * t6 * t2P + 2 * t4P * t7 * t7 * t2 * t2 * t1P * k * k * t1 * t1 * t3P * t3 + 4 * t4P * t7 * t7 * t2 * t1P * k
         * k * t1 * t1 * t3P * t4 * t3 - 8 * t4P * t5 * t1P * k * t13P * t7 * t3P - 12 * t4P * t5 * t12kP * t4 * t13P * t2 * t3 + 2 * t4P * t5
         * t1P * k * k * t1 * t1 * t6 * t2P * t4 + 16 * t5 * t12kP * k * t1 * t6 * t2P * t3 + 16 * t5 * t12kP * k * t1 * t6 * t2P * t4 + 8 * t5 * t12kP
         * k * t1 * t6 * t2P * t4 * t4 + 8 * t5 * t12kP * k * t1 * t6 * t2P - 8 * t5 * t5 * t12kP * k * t1 * t4P * t4 + 8 * t7 * t7 * t3P * t13P * t3
         * t1P * t4P + 4 * t5 * t12kP * k * t6 * t2P * t3 * t3 * t4 - 8 * t6 * t2P * t14P * t1P * t4P * t3 * t4 + 16 * t6 * t2P * t13P
         * t1P * t4P * t7 * t3 * t4 - 2 * t5 * t5 * t12kP * k * t4P * t3 * t3 * t4 + 24 * t7 * t3P * t2 * t1P * k * t1 * t4P - 12 * t6 * t6 * t2P
         * t1P * k * t1 * t4P * t4 + 4 * t7 * t3P * t1P * k * k * t1 * t1 * t4P * t6 * t2 * t4 - 3 * t4P * t5 * t12kP * t2 * t2 * t3 * t3 * t4 * t1 * t1 - 4
         * t6 * t2P * t1P * k * k * t4P * t3 - 8 * t6 * t2P * t13P * t1P * t4P * t2 * t3 * t3 * t4 + 4 * t6 * t2P * t14P * t1P * t4P
         * t7 * t3 * t3 + 6 * t5 * t12kP * k * t1 * t1 * t7 * t3P * t4 * t4 - 16 * t4P * t6 * t4 * t1P * k * t13P * t7 * t3P * t2 - 4 * t4P * t2 * t3
         * t4 * t1P * k * k * t6 * t2P + 18 * t4P * t1P * k * t1 * t1 * t6 * t2P + 24 * t4P * t1P * k * t1 * t6 * t2P * t3 + 4 * t4P * t7 * t14P
         * t2 * t4 * t1P * t6 * t2P + 16 * t4P * t7 * t13P * t2 * t4 * t1P * t6 * t2P * t3 + 8 * t4P * t2 * t4 * t5 * t1P * t14P * t6 * t2P
         * t3 + 4 * t4P * t2 * t2 * t4 * t5 * t1P * t14P * t7 * t3P - 12 * t4P * t7 * t2 * t1P * k * t6 * t2P * t1 * t3 * t3 * t4 - 18 * t4P * t4
         * t5 * t1P * k * t1 * t1 * t6 * t2P * t3 * t3 * t2 - 16 * t4P * t4 * t5 * t1P * k * t13P * t6 * t2P * t3 - 4 * t5 * t5 * t13kP * t2 * t1 * t1
         * t4 - 6 * t5 * t12kP * t13P * t4 * t4 * t6 * t2P * t3 + 12 * t5 * t12kP * t13P * t4 * t4P * t6 * t2 * t3 + 8 * t4P * t2 * t5 * t1P
         * k * k * t7 * t3P * t1 + 2 * t4P * t2 * t2 * t5 * t1P * k * k * t7 * t3P + 4 * t4P * t2 * t5 * t1P * k * k * t7 * t3P * t4 + 4 * t4P * t2 * t5
         * t1P * k * k * t6 * t2P * t1 * t3 * t3 + 2 * t4P * t2 * t5 * t1P * k * k * t6 * t2P * t3 * t3 * t4 - 3 * t5 * t12kP * t13P * t4 * t4P * t2
         * t2 + 2 * t7 * t3P * t1P * k * k * t1 * t1 * t4P * t6 * t2 * t2 * t4 + 2 * t7 * t3P * t1P * k * k * t1 * t1 * t4P * t6 * t4 - 4 * t4P * t2 * t3
         * t4 * t1P * k * k * t7 * t3P * t1 * t1 + 8 * t4P * t6 * t13P * t3 * t1P * t7 * t3P * t2 * t2 * t4 - 2 * t4P * t2 * t1P * k * k * t6 * t2P - 4
         * t4P * t2 * t1P * k * k * t7 * t3P - 18 * t6 * t2P * t1P * k * t1 * t1 * t4P * t7 * t4 + 24 * t5 * t12kP * t2 * t3 * k * t1 * t1 * t4P - 4
         * t4P * t2 * t3 * t4 * t1P * k * k * t7 * t3P - 6 * t5 * t12kP * t13P * t4 * t4 * t7 * t3P * t2 + 2 * t5 * t12kP * k * t6 * t2P * t3 * t3
         * t4 * t4 + 2 * t4P * t6 * t6 * t1P * k * k * t2P * t3 * t3 - 4 * t7 * t3P * t14P * t3 * t1P * t4P - 16 * t7 * t3P * t13P * t3 * t1P
         * t4P * t2 + 8 * t7 * t7 * t3P * t13P * t3 * t1P * t4P * t2 * t2 + 4 * t7 * t3P * t4 * t5 * t1P * k * k * t1 * t1 * t4P * t2 * t3 - 8 * t5
         * t12kP * k * t4P * t6 * t2 * t3 + 8 * t4P * t6 * t14P * t3 * t1P * t7 * t3P * t2 * t4 + 4 * t4P * t6 * t6 * t14P * t3 * t3 * t1P
         * t2P * t4 - 8 * t4P * t7 * t2 * t4 * t1P * k * t13P * t6 * t2P * t3 * t3 + 4 * t4P * t6 * t14P * t3 * t1P * t7 * t3P * t4 + 8 * t4P
         * t6 * t14P * t3 * t1P * t7 * t3P * t2 + 16 * t4P * t6 * t13P * t3 * t1P * t7 * t3P * t2 * t4 + 3 * t5 * t12kP * t13P * t4P
         * t6 - 3 * t5 * t12kP * t13P * t7 * t3P * t2 * t2 * t3 + 8 * t7 * t3P * t1P * k * k * t1 * t4P * t6 * t2 * t4 + 12 * t4P * t1P * k * t1
         * t6 * t2P + 12 * t4P * t1P * k * t1 * t7 * t3P + 8 * t7 * t3P * t5 * t12kP * k * t3 * t1 + 2 * t4P * t3 * t4 * t1P * k * t7 * t3P * t2
         * t2 + 4 * t4P * t3 * t4 * t1P * k * t6 * t2P * t2 - 4 * t5 * t5 * t13kP * t2 * t1 * t1 * t3 + 4 * t4P * t6 * t6 * t14P * t3 * t3 * t1P * t2P
         + 8 * t4P * t6 * t6 * t13P * t3 * t3 * t1P * t2P * t4 - 4 * t7 * t3P * t14P * t3 * t1P * t4P * t2 * t2 - 4 * t7 * t3P * t1P * k
         * k * t1 * t6 * t2P - 2 * t5 * t5 * t13kP * t2 * t2 * t1 * t1 * t3 + 12 * t5 * t12kP * t13P * t4 * t4P * t7 * t2 * t3 - 16 * t4P * t4 * t5 * t1P
         * k * t13P * t7 * t3P * t2 - 8 * t4P * t4 * t5 * t1P * k * t13P * t6 * t2P + 4 * t4P * t14P * t5 * t1P * t7 * t3P * t2 * t2 + 24
         * t6 * t2P * t1P * k * t1 * t7 * t3P * t4 + 6 * t5 * t12kP * t13P * t4 * t4P * t6 * t2 * t3 * t3 - 3 * t5 * t12kP * t13P * t4 * t4 * t6
         * t2P * t3 * t3 - 3 * t7 * t3P * t5 * t12kP * t2 * t2 * t1 * t1 + 18 * t7 * t3P * t1P * k * t1 * t1 * t4P * t3 * t4 - 6 * t7 * t3P * t5 * t12kP
         * t2 * t1 * t1 + 3 * t5 * t12kP * t13P * t4 * t4P * t7 * t3 * t3 + 6 * t5 * t12kP * t13P * t4 * t4P * t7 * t2 * t3 * t3 + 8 * t4P * t2 * t3
         * t3 * t1P * k * t13P * t6 * t2P + 8 * t5 * t12kP * t2 * k * t1 * t6 * t2P - 8 * t7 * t3P * t14P * t3 * t1P * t4P * t2 + 8 * t4P
         * t6 * t6 * t14P * t3 * t1P * t2P * t4 - 4 * t5 * t12kP * k * t4P * t6 * t2 * t2 * t3 + 8 * t5 * t12kP * k * t6 * t2P * t3 * t4 - 2 * t5 * t5
         * t13kP * t2 * t2 * t1 * t1 * t3 * t3 * t4 + 4 * t5 * t5 * t13kP * k * t2 * t1 + 8 * t5 * t5 * t13kP * k * t2 * t1 * t3 + 2 * t5 * t5 * t13kP * k * t2
         * t2 * t1 + 4 * t5 * t5 * t13kP * k * t2 * t2 * t1 * t3 + 8 * t5 * t5 * t13kP * k * t2 * t1 * t4 + 16 * t5 * t5 * t13kP * k * t2 * t1 * t3 * t4 + 4 * t5 * t5
         * t13kP * k * t2 * t2 * t1 * t4 + 8 * t5 * t5 * t13kP * k * t2 * t2 * t1 * t3 * t4 + 18 * t7 * t3P * t1P * k * t1 * t1 * t6 * t2P - 12 * t4P * t3
         * t3 * t5 * t1P * k * t1 * t6 * t2P - 24 * t4P * t3 * t5 * t1P * k * t1 * t6 * t2P - 12 * t4P * t3 * t5 * t1P * k * t1 * t7 * t3P - 16 * t6
         * t2P * t3 * t5 * t1P * k * t13P * t4P * t2 - 8 * t6 * t2P * t3 * t3 * t5 * t1P * k * t13P * t4P - 18 * t6 * t2P * t3 * t3 * t5 * t1P
         * k * t1 * t1 * t4P * t2 - 16 * t6 * t2P * t3 * t5 * t1P * k * t13P * t4P - 36 * t6 * t2P * t3 * t5 * t1P * k * t1 * t1 * t4P * t2 + 8 * t4P
         * t2 * t4 * t5 * t1P * t13P * t6 * t2P * t3 * t3 + 12 * t4P * t6 * t5 * t12kP * t2 * t3 * t1 * t1 + 2 * t6 * t6 * t2P * t2 * t4 * t1P * k
         * k * t4P - 4 * t6 * t2P * t2 * t1P * t1 * t1 * t7 * t3P * t4 * t4 + 36 * t7 * t3P * t1P * k * t1 * t1 * t4P * t2 + 18 * t7 * t3P * t1P
         * k * t1 * t1 * t4P * t2 * t2 * t3 + 8 * t7 * t3P * t1P * k * t13P * t4P * t2 * t2 + 16 * t4P * t6 * t6 * t13P * t3 * t1P * t2P * t2 - 12
         * t6 * t2P * t3 * t3 * t1P * k * t1 * t4P * t7 * t4 + 2 * t6 * t2P * t2 * t3 * t1P * k * t7 * t3P * t4 * t4 - 8 * t5 * t12kP * t3 * t3 * t4
         * k * t1 * t4P * t7 + 4 * t4P * t6 * t2 * t2 * t1P * t14P * t7 * t3P * t4 + 8 * t4P * t6 * t6 * t2 * t1P * t1 * t1 * t2P * t3 * t4 + 8 * t4P
         * t7 * t7 * t13P * t2 * t2 * t4 * t1P * t3P * t3 + 6 * t4P * t6 * t5 * t12kP * t2 * t3 * t3 * t1 * t1 + 6 * t4P * t6 * t5 * t12kP * t2 * t2
         * t3 * t1 * t1 + 3 * t4P * t6 * t5 * t12kP * t2 * t2 * t3 * t3 * t1 * t1 - 8 * t4P * t2 * t2 * t5 * t1P * k * t13P * t7 * t3P - 8 * t4P * t2
         * t5 * t1P * k * t13P * t6 * t2P + 2 * t7 * t7 * t3P * t1P * k * k * t1 * t1 * t4P * t3 + 8 * t6 * t2P * t13P * t1P * t4P * t7 * t3
         * t3 + 8 * t6 * t2P * t13P * t1P * t4P * t7 * t2 * t3 * t3 + 4 * t4P * t2 * t3 * t1P * k * t7 * t3P + 4 * t4P * t7 * t7 * t2 * t2 * t1P
         * k * k * t1 * t3P * t4 * t3 - 8 * t7 * t3P * t1P * k * t13P * t4P * t6 - 18 * t7 * t3P * t1P * k * t1 * t1 * t4P * t6 * t3 - 18 * t7 * t7
         * t3P * t1P * k * t1 * t1 * t4P * t3 + 8 * t4P * t6 * t1P * t13P * t7 * t3P * t4 - 18 * t7 * t3P * t1P * k * t1 * t1 * t4P * t6
         * t2 * t2 + 18 * t7 * t3P * t1P * k * t1 * t1 * t6 * t2P * t2 - 8 * t7 * t7 * t3P * t1P * k * t13P * t4P + 4 * t4P * t4 * t5 * t1P * t1
         * t1 * t6 * t2P + 4 * t4P * t4 * t5 * t1P * t1 * t1 * t7 * t3P + 4 * t5 * t12kP * k * t6 * t2P * t4 + 8 * t4P * t6 * t6 * t2 * t1P * t1
         * t1 * t2P * t3 + 4 * t4P * t6 * t6 * t2 * t1P * t1 * t1 * t2P * t4 - 18 * t4P * t4 * t5 * t1P * k * t1 * t1 * t6 * t2P - 24 * t4P * t4
         * t5 * t1P * k * t1 * t6 * t2P * t3 - 12 * t4P * t4 * t5 * t1P * k * t1 * t6 * t2P - 2 * t6 * t6 * t22kP * t4 * t4 * t1P * k * k * t1 + 4 * t42kP
         * t2 * t5 * t1P * k * k - t5 * t5 * t13kP * t2 * t2 * t1 * t1 - 2 * t5 * t5 * t13kP * t4 * t1 * t1 - 2 * t5 * t5 * t13kP * t2 * t1 * t1 - 3 * t5 * t12kP
         * t13P * t4P + 3 * t5 * t5 * t12kP * t1 * t1 * t4P - 3 * t5 * t12kP * t1 * t1 * t4P - t5 * t5 * t13kP * t4 * t4 * t1 * t1 - 2 * t5 * t5
         * t12kP * k * t4P + 2 * t5 * t12kP * k * t4P - 8 * t42kP * t7 * t2 * t3 * t3 * t1P * t1 * t1 * t5 + 8 * t42kP * t6 * t2 * t1P * t1 * t1
         * t3 * t3 + t4P * t7 * t2 * t2 * t12kP * k * k * t1 * t5 * t3 * t3 * t4 - 16 * t42kP * t2 * t5 * t1P * k * t13P - t6 * t6 * t22kP * t1P
         * k * k * t1 * t1 - 16 * t42kP * t13P * t5 * t5 * t1P * t2 * t3 + 8 * t42kP * t14P * t5 * t1P * t2 + 4 * t5 * t5 * t13kP * t2 * t4 * t4
         * k * t3 - 8 * t42kP * t6 * t6 * t14P * t3 * t1P * t2 + 8 * t42kP * t13P * t1P * t7 * t2 * t2 + 2 * t42kP * t7 * t7 * t2 * t2 * t1P
         * k * t3 - 2 * t42kP * t2 * t2 * t5 * t5 * t1P * t1 * t1 + t4P * t5 * t5 * t12kP * k * k * t1 * t3 * t3 * t4 - 16 * t42kP * t6 * t14P * t3 * t1P
         * t7 * t2 - 2 * t42kP * t5 * t5 * t1P * t1 * t1 + 6 * t4P * t5 * t5 * t12kP * t2 * t1 * t1 - 3 * t5 * t12kP * t13P * t4 * t4 * t7 * t3P
         * t2 * t2 * t3 - 4 * t42kP * t6 * t2 * t2 * t1P * t1 * t1 * t7 - 16 * t42kP * t13P * t5 * t1P * t6 * t2 * t3 * t3 + 72 * t42kP * t7 * t3 * t5
         * t1P * k * t1 * t1 * t2 - 2 * t7 * t7 * t32kP * t4 * t4 * t1P * t1 * t1 - 4 * t42kP * t2 * t1P * k * k * t1 - t6 * t6 * t22kP * t1P * k * k
         + t6 * t6 * t22kP * t4 * t4 * t1P * k * t3 * t3 - 2 * t42kP * t7 * t7 * t1P * t1 * t1 * t3 * t3 + 4 * t42kP * t6 * t2 * t2 * t1P * t1 * t1 * t3
         * t3 + 16 * t7 * t3P * t5 * t12kP * k * t2 * t3 * t1 + 8 * t7 * t3P * t5 * t12kP * k * t2 * t2 * t3 * t1 - 4 * t4P * t1P * t1 * t1 * t6 * t2P - 8
         * t4P * t7 * t7 * t2 * t2 * t4 * t1P * k * t13P * t3P + 4 * t4P * t6 * t12kP * k * k * t5 * t2 * t3 * t4 * t1 + 4 * t42kP * t14P * t1P
         * t7 + 4 * t42kP * t5 * t5 * t1P * k * t13P + 2 * t42kP * t5 * t1P * k * t6 * t3 * t3 - 4 * t6 * t6 * t22kP * t4 * t4 * t1P * t1 * t1 * t3
         + 4 * t42kP * t2 * t2 * t3 * t3 * t1P * t1 * t1 * t5 - 4 * t42kP * t6 * t6 * t2 * t2 * t1P * t13P - t6 * t2P * t3 * t3 * t12kP * k * k * t1
         * t5 * t4 * t4 - 2 * t6 * t6 * t22kP * t1P * t1 * t1 - 2 * t42kP * t6 * t1P * k + 4 * t42kP * t14P * t5 * t1P + 8 * t42kP * t14P
         * t1P * t7 * t2 - 2 * t42kP * t2 * t2 * t5 * t1P * k * k * t7 * t3 * t3 - 2 * t7 * t3P * t12kP * k * k * t1 * t5 * t2 * t3 - 4 * t42kP * t6 * t2
         * t2 * t1P * t1 * t1 * t5 + 8 * t42kP * t2 * t1P * k * k * t6 * t1 - 2 * t42kP * t7 * t7 * t2 * t2 * t1P * k * k * t1 - t7 * t3P * t12kP * k
         * k * t1 * t5 * t2 * t2 + 8 * t42kP * t14P * t5 * t1P * t2 * t2 * t3 - 3 * t7 * t3P * t5 * t12kP * t2 * t2 * t3 * t4 * t4 * t1 * t1 + 8 * t42kP
         * t2 * t2 * t5 * t1P * k * t13P * t7 + 32 * t42kP * t7 * t2 * t1P * k * t6 * t13P * t3 - t4P * t2 * t2 * t12kP * k * k * t5 * t3 * t3 + 4
         * t5 * t5 * t13kP * t2 * t4 * k * t3 * t3 - 8 * t42kP * t6 * t2 * t1P * t14P * t7 - 2 * t6 * t2P * t3 * t4 * t4 * t12kP * k * k * t5 - t4P
         * t2 * t2 * t12kP * k * k * t5 * t4 - 2 * t42kP * t7 * t2 * t2 * t1P * k * k * t1 * t1 * t6 * t3 * t3 - 18 * t42kP * t2 * t2 * t5 * t1P * k * t1 * t1
         + 12 * t42kP * t6 * t6 * t2 * t2 * t3 * t1P * k * t1 + 16 * t42kP * t7 * t2 * t1P * k * t6 * t13P + t4P * t2 * t2 * t5 * t5 * t12kP * k
         * k * t1 + 8 * t6 * t2P * t3 * t1P * t1 * t1 * t4P * t5 + 4 * t7 * t7 * t3P * t1P * k * k * t1 * t4P - 2 * t4P * t2 * t2 * t3 * t4 * t1P * k
         * k * t7 * t3P + 4 * t4P * t7 * t1P * t1 * t1 * t6 * t2P * t3 * t3 + 9 * t42kP * t6 * t6 * t1P * k * t1 * t1 * t2 * t2 - 2 * t7 * t3P * t12kP
         * k * k * t1 * t5 * t2 * t2 * t3 * t4 - 2 * t6 * t2P * t3 * t12kP * k * k * t1 * t5 * t4 * t4 - 36 * t42kP * t6 * t2 * t3 * t3 * t1P * k * t1 * t1 + 4 * t42kP
         * t2 * t5 * t1P * k * k * t1 * t1 + 8 * t42kP * t7 * t7 * t2 * t1P * k * t13P + 6 * t42kP * t2 * t2 * t3 * t3 * t1P * k * t1 - 18 * t4P * t6
         * t6 * t4 * t1P * k * t1 * t1 * t2P * t2 - 4 * t4P * t7 * t7 * t2 * t1P * k * t3P * t3 + 2 * t4P * t7 * t7 * t2 * t2 * t1P * k * k * t1 * t1 * t3P
         * t4 - 24 * t4P * t6 * t6 * t2 * t3 * t1P * k * t1 * t2P - 6 * t4P * t5 * t12kP * t2 * t2 * t3 * t4 * t1 * t1 + 12 * t7 * t3P * t2 * t2 * t1P
         * k * t1 * t4P * t4 - 16 * t7 * t3P * t13P * t3 * t1P * t6 * t2P * t4 + 4 * t4P * t6 * t6 * t2 * t1P * t14P * t2P + 6 * t5 * t12kP
         * t1 * t1 * t4P * t6 * t2 * t2 * t3 * t4 - 3 * t5 * t12kP * t1 * t1 * t7 * t3P * t2 * t2 * t4 * t4 + 3 * t5 * t12kP * t1 * t1 * t4P * t6 * t2 * t2
         * t4 + 3 * t5 * t12kP * t1 * t1 * t4P * t6 * t3 * t3 * t4 - 3 * t5 * t12kP * t1 * t1 * t4P * t2 * t2 * t4 + 24 * t7 * t3P * t2 * t1P * k * t1
         * t4P * t4 + 4 * t4P * t6 * t6 * t2 * t1P * t14P * t2P * t4 - 8 * t4P * t6 * t5 * t12kP * k * t4 * t1 + 8 * t4P * t2 * t5 * t1P * k
         * k * t7 * t3P * t1 * t3 + 2 * t4P * t2 * t2 * t5 * t1P * k * k * t7 * t3P * t3 + 4 * t4P * t2 * t5 * t1P * k * k * t7 * t3P * t4 * t3 + 2 * t4P
         * t2 * t5 * t1P * k * k * t6 * t2P * t3 * t3 * t1 * t1 + 4 * t4P * t2 * t5 * t1P * k * k * t7 * t3P * t1 * t1 * t3 + 2 * t4P * t2 * t2 * t5 * t1P
         * k * k * t7 * t3P * t1 * t1 * t3 + 2 * t4P * t4 * t1P * k * t6 * t2P + 2 * t4P * t4 * t1P * k * t7 * t3P - 2 * t4P * t7 * t2 * t1P * k
         * t6 * t2P * t3 * t3 - 32 * t5 * t12kP * t3 * t4 * k * t1 * t4P * t6 * t2 - 2 * t7 * t3P * t2 * t2 * t3 * t1P * k * k * t4P + 8 * t5 * t12kP
         * t3 * t4 * t4 * k * t1 * t7 * t3P * t2 * t2 + 16 * t4P * t2 * t4 * t5 * t1P * t13P * t6 * t2P * t3 + 8 * t4P * t2 * t2 * t4 * t5 * t1P * t13P
         * t7 * t3P + 4 * t5 * t12kP * k * t6 * t2P * t3 * t2 + 4 * t4P * t6 * t2 * t2 * t1P * t14P * t7 * t3P - 8 * t4P * t6 * t6 * t4 * t1P
         * k * t13P * t2P - 16 * t7 * t3P * t13P * t3 * t1P * t6 * t2P * t4 * t2 + 16 * t4P * t5 * t12kP * k * t2 * t1 + 32 * t4P * t5 * t12kP
         * k * t2 * t1 * t3 + 24 * t4P * t3 * t4 * t1P * k * t7 * t3P * t1 * t2 - 18 * t4P * t4 * t5 * t1P * k * t1 * t1 * t7 * t3P * t2 * t2 * t3 - 4 * t4P
         * t6 * t1P * k * t7 * t3P * t2 + 4 * t7 * t3P * t1P * k * k * t1 * t4P * t6 * t2 * t2 * t3 * t4 + 24 * t5 * t12kP * t2 * t3 * k * t1 * t1 * t6 * t2P
         * t4 + 6 * t5 * t12kP * t2 * t3 * t3 * k * t1 * t1 * t6 * t2P - 36 * t4P * t6 * t6 * t4 * t1P * k * t1 * t1 * t2P * t3 - 16 * t5 * t12kP * t3
         * t3 * t4 * k * t1 * t4P * t7 * t2 - 16 * t4P * t7 * t7 * t2 * t1P * k * t3P * t13P * t3 - 24 * t4P * t6 * t2 * t3 * t1P * k * t1 * t7 * t3P - 16
         * t4P * t6 * t2 * t3 * t1P * k * t13P * t7 * t3P + 12 * t6 * t2P * t1P * k * t1 * t7 * t3P - 36 * t4P * t7 * t7 * t2 * t1P * k * t3P
         * t1 * t1 * t3 - 8 * t6 * t2P * t13P * t1P * t7 * t3P * t4 * t4 - 16 * t4P * t2 * t5 * t1P * k * t13P * t7 * t3P - 8 * t7 * t7 * t3P
         * t1P * k * t13P * t4P * t4 + 36 * t7 * t3P * t1P * k * t1 * t1 * t6 * t2P * t3 * t4 * t2 - 2 * t4P * t6 * t1P * k * t7 * t3P * t3 + 4
         * t7 * t3P * t1P * k * k * t1 * t4P * t6 * t2 * t2 * t4 - 8 * t7 * t3P * t13P * t3 * t1P * t4P * t2 * t2 * t4 + 12 * t5 * t12kP * t2 * t3
         * k * t1 * t1 * t6 * t2P + 36 * t7 * t3P * t1P * k * t1 * t1 * t4P * t2 * t4 - 2 * t4P * t6 * t1P * k * t7 * t3P * t4 - 12 * t4P * t4 * t5
         * t1P * k * t1 * t7 * t3P * t2 * t2 - 24 * t4P * t4 * t5 * t1P * k * t1 * t6 * t2P * t3 * t2 + 8 * t5 * t12kP * t3 * t3 * t4 * k * t1 * t4P - 8
         * t5 * t5 * t12kP * t3 * t3 * t4 * k * t1 * t4P * t2 * t2 + 2 * t7 * t3P * t2 * t2 * t4 * t1P * k * k * t4P * t6 - 8 * t5 * t12kP * t2 * t2 * k
         * t1 * t4P * t7 - 32 * t5 * t12kP * t2 * k * t1 * t4P * t7 * t3 - 8 * t4P * t4 * t5 * t1P * k * t13P * t7 * t3P * t2 * t2 - 12 * t5 * t12kP
         * k * t1 * t1 * t4P * t7 * t2 + 2 * t7 * t7 * t3P * t1P * k * k * t4P * t3 * t4 + 4 * t6 * t2P * t3 * t3 * t1P * k * k * t1 * t4P * t7 * t4 - 24
         * t7 * t3P * t2 * t1P * k * t1 * t4P * t6 * t4 + 12 * t5 * t12kP * t2 * t3 * k * t1 * t1 * t7 * t3P - 6 * t5 * t12kP * k * t1 * t1 * t4P * t6
         * t2 * t2 + 6 * t5 * t12kP * k * t1 * t1 * t6 * t2P * t2 - 16 * t4P * t5 * t5 * t12kP * k * t2 * t4 * t1 + 4 * t5 * t12kP * t2 * t4 * k * t6 * t2P
         + 8 * t5 * t12kP * t2 * t4 * k * t7 * t3P - 2 * t5 * t12kP * t2 * t2 * t4 * k * t4P * t6 + 8 * t5 * t12kP * t2 * t4 * k * t4P * t3 - 8 * t4P
         * t7 * t7 * t2 * t2 * t1P * k * t3P * t13P + 12 * t5 * t12kP * t2 * t3 * t3 * k * t1 * t1 * t4P - 12 * t5 * t12kP * t2 * t2 * t3 * k * t1 * t1
         * t4P * t6 - 2 * t4P * t5 * t1P * k * t6 * t2P - 2 * t4P * t5 * t1P * k * t7 * t3P - 36 * t4P * t4 * t5 * t1P * k * t1 * t1 * t7 * t3P
         * t2 * t3 + 4 * t4P * t7 * t7 * t2 * t2 * t3 * t1P * t1 * t1 * t3P * t4 + 12 * t4P * t4 * t1P * k * t7 * t3P * t1 + 4 * t4P * t4 * t1P * k
         * t7 * t3P * t2 + 6 * t5 * t12kP * t3 * t3 * t4 * k * t1 * t1 * t4P - 18 * t4P * t2 * t2 * t5 * t1P * k * t1 * t1 * t7 * t3P + 2 * t6 * t2P
         * t2 * t3 * t3 * t1P * k * k * t4P * t7 + 2 * t4P * t6 * t6 * t1P * k * k * t2P * t4 * t1 * t1 + 2 * t4P * t6 * t6 * t1P * k * k * t2P * t1
         * t1 + 2 * t4P * t6 * t6 * t1P * k * k * t2P * t1 * t1 * t2 - 12 * t4P * t5 * t1P * k * t7 * t3P * t1 - 4 * t4P * t5 * t1P * k * t7 * t3P
         * t2 + 3 * t4P * t5 * t5 * t12kP * t2 * t2 * t4 * t1 * t1 * t3 * t3 + 4 * t4P * t6 * t6 * t3 * t3 * t1P * t1 * t1 * t2P * t4 - 8 * t6 * t2P * t3
         * t3 * t1P * k * t13P * t4P * t7 * t4 + 16 * t5 * t12kP * t3 * t3 * t4 * k * t1 * t4P * t2 + 12 * t5 * t12kP * t3 * t4 * k * t1 * t1 * t7 * t3P - 24
         * t4P * t3 * t5 * t1P * k * t1 * t6 * t2P * t2 - 12 * t4P * t3 * t5 * t1P * k * t1 * t7 * t3P * t2 * t2 + 2 * t6 * t2P * t2 * t3 * t1P * k
         * t7 * t3P + 12 * t4P * t3 * t4 * t1P * k * t7 * t3P * t1 + 4 * t4P * t3 * t4 * t1P * k * t7 * t3P * t2 + 16 * t6 * t2P * t5 * t12kP
         * k * t2 * t4 * t1 + 8 * t6 * t2P * t5 * t12kP * k * t2 * t4 * t4 * t1 + 4 * t5 * t5 * t13kP * k * t3 * t4 * t4 * t1 + 4 * t6 * t2P * t3 * t1P * k * k
         * t1 * t1 * t4P * t5 * t4 + 6 * t5 * t12kP * t2 * t2 * t3 * t3 * k * t1 * t1 * t4P + 6 * t5 * t12kP * t2 * t2 * t3 * k * t1 * t1 * t7 * t3P - 3 * t5
         * t12kP * t13P * t4 * t4 * t6 * t2P * t3 * t3 * t2 + 18 * t7 * t3P * t1P * k * t1 * t1 * t6 * t2P * t3 * t4 * t4 * t2 - 2 * t7 * t3P * t1P
         * k * k * t1 * t1 * t6 * t2P * t4 * t4 * t2 + 6 * t5 * t12kP * t3 * t3 * t4 * t4 * k * t1 * t1 * t6 * t2P - 3 * t5 * t12kP * t13P * t4 * t4P * t2
         * t2 * t3 * t3 + 3 * t5 * t12kP * t13P * t4 * t4P * t7 + 3 * t5 * t5 * t12kP * t13P * t4 * t4P + 2 * t4P * t5 * t1P * k * k * t6 * t2P
         * t3 * t3 - 3 * t5 * t12kP * t13P * t4 * t4 * t7 * t3P * t2 * t2 + 3 * t4P * t5 * t5 * t12kP * t2 * t2 * t4 * t13P * t3 * t3 - 4 * t6 * t2P
         * t2 * t4 * t1P * k * k * t7 * t3P * t3 - 12 * t4P * t3 * t3 * t5 * t1P * k * t1 * t6 * t2P * t2 - 6 * t5 * t12kP * t13P * t4 * t6 * t2P - 6
         * t5 * t12kP * t13P * t4 * t7 * t3P - 12 * t5 * t12kP * t13P * t4 * t6 * t2P * t3 - 3 * t5 * t12kP * t13P * t4 * t4 * t6 * t2P
         + 3 * t5 * t12kP * t13P * t4 * t4P * t6 * t2 * t2 * t3 * t3 + 24 * t5 * t12kP * t2 * t3 * k * t1 * t1 * t7 * t3P * t4 + 12 * t6 * t2P * t5 * t12kP
         * k * t2 * t4 * t1 * t1 + 6 * t6 * t2P * t5 * t12kP * k * t2 * t4 * t4 * t1 * t1 + 8 * t4P * t6 * t6 * t14P * t3 * t1P * t2P + 8 * t4P * t6
         * t6 * t13P * t3 * t3 * t1P * t2P + 16 * t4P * t6 * t6 * t13P * t3 * t1P * t2P * t4 + 3 * t5 * t12kP * t13P * t4 * t4P * t6 - 6
         * t5 * t12kP * t13P * t4 * t7 * t3P * t2 * t2 * t3 - 12 * t5 * t5 * t12kP * t2 * t2 * k * t1 * t1 * t4P * t3 - 8 * t6 * t2P * t3 * t1P * k
         * k * t1 * t4P * t2 * t4 - 2 * t6 * t2P * t2 * t1P * k * t4P * t5 + 12 * t5 * t12kP * t2 * t2 * t3 * k * t1 * t1 * t4P + 4 * t4P * t14P
         * t5 * t1P * t6 * t2P * t3 * t3 + 8 * t4P * t13P * t5 * t1P * t6 * t2P * t3 * t3 * t4 - 16 * t5 * t5 * t12kP * t2 * k * t1 * t4P * t3
         * t3 + 4 * t4P * t14P * t5 * t1P * t7 * t3P * t2 * t2 * t3 + 8 * t4P * t14P * t5 * t1P * t7 * t3P * t2 * t3 + 4 * t4P * t14P * t5
         * t1P * t6 * t2P * t3 * t3 * t4 + 2 * t7 * t7 * t3P * t2 * t2 * t3 * t4 * t1P * k * k * t4P + 4 * t4P * t2 * t4 * t5 * t1P * t1 * t1 * t6 * t2P
         + 8 * t4P * t2 * t4 * t5 * t1P * t1 * t1 * t7 * t3P + 16 * t4P * t2 * t4 * t5 * t1P * t13P * t7 * t3P + 4 * t4P * t2 * t2 * t4 * t5 * t1P
         * t1 * t1 * t7 * t3P - 2 * t4P * t2 * t3 * t3 * t5 * t1P * k * t6 * t2P * t4 - 2 * t4P * t2 * t2 * t3 * t5 * t1P * k * t7 * t3P * t4 + 4 * t6
         * t6 * t2P * t3 * t4 * t1P * k * k * t4P + 4 * t6 * t2P * t3 * t4 * t1P * k * k * t4P * t5 - 3 * t5 * t12kP * t13P * t4 * t4 * t7 * t3P
         + 18 * t4P * t3 * t3 * t4 * t1P * k * t6 * t2P * t1 * t1 * t2 + 6 * t5 * t12kP * t13P * t4 * t4P * t6 * t2 + 3 * t4P * t7 * t5 * t12kP
         * t2 * t2 * t1 * t1 - t5 * t5 * t13kP * t2 * t2 * t1 * t1 * t3 * t3 + 8 * t4P * t5 * t12kP * k * t2 * t2 * t1 + 16 * t4P * t5 * t12kP * k * t2 * t2
         * t1 * t3 + 12 * t4P * t5 * t12kP * k * t2 * t1 * t1 + 6 * t4P * t5 * t12kP * k * t2 * t2 * t1 * t1 + 2 * t4P * t2 * t5 * t1P * k * k * t6 * t2P
         * t3 * t3 + 4 * t4P * t2 * t5 * t1P * k * k * t7 * t3P * t3 + 2 * t4P * t2 * t5 * t1P * k * k * t6 * t2P * t3 * t3 * t4 * t1 * t1 - 24 * t4P * t6
         * t2 * t3 * t1P * k * t1 * t7 * t3P * t4 + 8 * t4P * t2 * t4 * t5 * t1P * t13P * t6 * t2P + 8 * t4P * t2 * t4 * t5 * t1P * t1 * t1 * t6
         * t2P * t3 - 3 * t5 * t12kP * t13P * t6 * t2P * t3 * t3 * t2 - 8 * t4P * t6 * t6 * t4 * t1P * k * t13P * t2P * t2 - 36 * t4P * t6
         * t2 * t3 * t1P * k * t1 * t1 * t7 * t3P - 12 * t4P * t6 * t2 * t2 * t3 * t1P * k * t1 * t7 * t3P - 36 * t4P * t6 * t6 * t4 * t1P * k * t1 * t1
         * t2P * t3 * t2 - 16 * t4P * t6 * t5 * t12kP * k * t3 * t1 + 8 * t4P * t1P * k * t13P * t6 * t2P + 8 * t4P * t1P * k * t13P * t6
         * t2P * t2 + 12 * t7 * t3P * t2 * t1P * k * t1 * t6 * t2P * t4 * t4 - 2 * t4P * t1P * k * k * t1 * t1 * t6 * t2P * t4 + 2 * t7 * t7 * t3P
         * t2 * t2 * t3 * t1P * k * k * t4P - 3 * t5 * t12kP * t13P * t4 * t4 * t7 * t3P * t3 - 12 * t5 * t12kP * t13P * t4 * t7 * t3P * t2 * t3 - 6
         * t5 * t12kP * t13P * t4 * t4P * t2 * t3 * t3 - 12 * t5 * t12kP * t13P * t4 * t7 * t3P * t2 + 3 * t5 * t12kP * t13P * t4P * t7 - 6
         * t5 * t12kP * t13P * t4 * t4P * t2 * t2 * t3 + 3 * t5 * t12kP * t13P * t4 * t4P * t6 * t2 * t2 - 16 * t7 * t3P * t3 * t5 * t1P * k
         * t13P * t4P * t2 - 8 * t7 * t3P * t3 * t5 * t1P * k * t13P * t4P + 8 * t7 * t3P * t1P * k * t13P * t6 * t2P * t4 * t4 * t2 - 16
         * t4P * t6 * t5 * t12kP * k * t2 * t1 + 3 * t5 * t12kP * t13P * t4 * t4P * t7 * t2 * t2 - 6 * t5 * t12kP * t13P * t4 * t7 * t3P * t2
         * t2 - 8 * t7 * t3P * t13P * t3 * t1P * t6 * t2P - 2 * t4P * t2 * t2 * t1P * k * k * t7 * t3P * t4 + 2 * t7 * t7 * t3P * t2 * t2 * t4 * t1P
         * k * k * t4P + 12 * t6 * t2P * t3 * t1P * k * t1 * t7 * t3P * t4 * t4 - 2 * t4P * t2 * t2 * t3 * t5 * t1P * k * t7 * t3P - 4 * t4P * t2 * t3
         * t5 * t1P * k * t7 * t3P * t4 - 6 * t5 * t12kP * t13P * t4 * t7 * t3P * t3 - 6 * t5 * t12kP * t13P * t4 * t6 * t2P * t3 * t3 - 8 * t5
         * t5 * t12kP * t2 * t2 * k * t1 * t4P * t3 * t3 + 6 * t5 * t12kP * t3 * t3 * k * t1 * t1 * t4P - 6 * t5 * t5 * t12kP * t2 * t2 * k * t1 * t1 * t4P
         * t3 * t3 - 12 * t5 * t5 * t12kP * t2 * k * t1 * t1 * t4P * t3 * t3 + 2 * t4P * t4 * t1P * k * t7 * t3P * t2 * t2 - 36 * t7 * t3P * t3 * t5 * t1P
         * k * t1 * t1 * t4P * t2 - 18 * t7 * t3P * t3 * t5 * t1P * k * t1 * t1 * t4P + 24 * t42kP * t3 * t5 * t1P * k * t1 * t6 - 2 * t4P * t2 * t12kP
         * k * k * t5 * t4 + 12 * t42kP * t6 * t6 * t2 * t3 * t3 * t1P * k * t1 + t42kP * t6 * t6 * t1P * k * t3 * t3 - 12 * t42kP * t1P * k * t1 * t7 - 8
         * t42kP * t6 * t3 * t3 * t5 * t1P * k * k * t1 * t2 + 16 * t42kP * t7 * t2 * t3 * t1P * t14P - 4 * t42kP * t14P * t1P * t2 * t2 * t3
         + 2 * t42kP * t1P * k * k * t1 * t1 * t7 * t3 * t3 - 16 * t42kP * t1P * k * t13P * t6 * t2 + 2 * t4P * t6 * t12kP * k * k * t5 * t2 * t1 - 8
         * t42kP * t2 * t1P * k * k * t1 * t3 + 2 * t6 * t6 * t22kP * t3 * t1P * k + 2 * t4P * t6 * t12kP * k * k * t5 * t2 * t3 * t3 + 12 * t6 * t6 * t22kP
         * t3 * t1P * k * t1 * t4 * t4 + 9 * t7 * t7 * t32kP * t1P * k * t1 * t1 + 8 * t42kP * t6 * t14P * t3 * t1P - 8 * t42kP * t13P * t5 * t5
         * t1P * t2 * t2 * t3 - t6 * t2P * t2 * t4 * t4 * t12kP * k * k * t5 * t1 + 2 * t42kP * t7 * t7 * t2 * t1P * k * t3 * t3 + 18 * t42kP * t1P
         * k * t1 * t1 * t2 + 6 * t42kP * t5 * t5 * t1P * k * t1 + t4P * t6 * t12kP * k * k * t5 * t1 - t4P * t12kP * k * k * t1 * t5 + 8 * t42kP * t5
         * t5 * t3 * t1P * k * t13P + 2 * t4P * t5 * t5 * t12kP * k * k * t1 * t3 * t4 + t42kP * t7 * t7 * t1P * k * t3 * t3 - t42kP * t5 * t5 * t1P
         * k * k * t1 * t1 * t3 * t3 + t42kP * t5 * t5 * t1P * k * t2 * t2 - 16 * t42kP * t6 * t2 * t1P * t1 * t1 * t5 * t3 - 4 * t4P * t2 * t3 * t4 * t12kP
         * k * k * t5 * t1 - 18 * t4P * t4 * t5 * t1P * k * t1 * t1 * t6 * t2P * t2 - 4 * t42kP * t6 * t2 * t2 * t1P * t14P * t7 + 16 * t7 * t3P * t5
         * t12kP * k * t1 * t4 - 4 * t42kP * t7 * t2 * t2 * t3 * t3 * t1P * t1 * t1 * t5 - 2 * t42kP * t7 * t7 * t1P * t14P * t3 * t3 - 4 * t42kP
         * t3 * t5 * t5 * t1P * t1 * t1 - 4 * t42kP * t7 * t7 * t2 * t1P * k * k * t1 * t3 * t3 + 32 * t42kP * t7 * t2 * t3 * t1P * t13P - 8 * t42kP
         * t6 * t2 * t1P * t1 * t1 * t7 + t4P * t7 * t3 * t3 * t12kP * k * k * t5 * t1 + t4P * t6 * t12kP * k * k * t5 * t2 * t2 * t3 * t3 + 18 * t6 * t6 * t22kP
         * t3 * t1P * k * t1 * t1 * t4 * t4 - 4 * t42kP * t14P * t5 * t1P * t7 * t3 * t3 - 4 * t6 * t6 * t22kP * t4 * t1P * k * k * t1 - t4P * t2 * t2
         * t12kP * k * k * t5 + t4P * t6 * t12kP * k * k * t5 * t2 * t2 * t3 * t3 * t1 - 2 * t4P * t2 * t2 * t12kP * k * k * t5 * t3 * t1 - 8 * t42kP * t1P
         * k * t13P * t7 * t3 * t3 - 4 * t7 * t7 * t32kP * t1P * k * k * t1 * t4 - 4 * t42kP * t2 * t1P * k * k * t3 * t1 * t1 - 4 * t42kP * t13P * t5
         * t5 * t1P * t2 * t2 - 8 * t42kP * t7 * t2 * t1P * k * k * t1 * t6 * t3 * t3 - 2 * t6 * t6 * t22kP * t14P * t1P * t3 * t3 + 16 * t6 * t6 * t22kP
         * t3 * t1P * k * t13P * t4 - 2 * t7 * t7 * t32kP * t4 * t4 * t1P * t14P + t42kP * t1P * k - 2 * t42kP * t2 * t2 * t1P * k * k * t3
         * t1 * t1 - 8 * t42kP * t7 * t2 * t2 * t3 * t5 * t1P * k * k * t1 - 2 * t6 * t6 * t22kP * t1P * k * k * t3 - 8 * t42kP * t6 * t1P * k * k * t7 * t2
         * t3 + 2 * t5 * t5 * t13kP * k * t1 + 2 * t42kP * t7 * t7 * t1P * k * t3 - 12 * t42kP * t3 * t3 * t5 * t1P * k * t1 * t2 * t2 + 2 * t4P * t5 * t5
         * t12kP * k * k * t3 * t4 + 24 * t42kP * t7 * t3 * t3 * t5 * t1P * k * t1 * t2 + 36 * t42kP * t7 * t2 * t1P * k * t6 * t1 * t1 - 2 * t42kP * t6
         * t1P * k * t2 * t2 - t7 * t7 * t32kP * t2 * t2 * t4 * t4 * t1P * k * k + 8 * t42kP * t6 * t14P * t3 * t1P * t2 * t2 - t42kP * t2 * t2 * t5
         * t5 * t1P * k * k * t3 * t3 + 2 * t4P * t2 * t2 * t5 * t5 * t12kP * k * k * t3 * t1 + 16 * t42kP * t13P * t5 * t1P * t2 * t3 * t3 + 2 * t42kP
         * t7 * t2 * t2 * t1P * k * k * t1 * t1 * t3 * t3 - 12 * t42kP * t7 * t2 * t2 * t1P * k * t1 + 16 * t42kP * t2 * t5 * t1P * k * t13P * t7 - 4 * t42kP
         * t14P * t5 * t5 * t1P * t2 * t3 * t3 - 8 * t42kP * t6 * t2 * t2 * t1P * t1 * t1 * t5 * t3 - 8 * t42kP * t6 * t14P * t3 * t1P * t7 * t2
         * t2 + 2 * t7 * t7 * t32kP * t4 * t1P * k + 8 * t42kP * t6 * t6 * t3 * t1P * k * t13P + 2 * t4P * t6 * t12kP * k * k * t5 * t2 * t2 * t3 * t4
         * t1 - 2 * t42kP * t14P * t5 * t5 * t1P * t2 * t2 * t3 * t3 - 4 * t7 * t7 * t32kP * t2 * t4 * t1P * k * k - 8 * t42kP * t2 * t2 * t5 * t1P
         * k * t13P + 8 * t42kP * t2 * t1P * t1 * t1 * t5 + 8 * t7 * t7 * t32kP * t1P * k * t13P * t4 - 4 * t42kP * t2 * t2 * t5 * t1P * k * k
         * t7 * t1 + 2 * t4P * t6 * t12kP * k * k * t5 * t2 * t2 * t3 * t1 - t42kP * t2 * t2 * t1P * k * k * t1 * t1 + 4 * t42kP * t14P * t5 * t1P
         * t2 * t2 * t3 * t3 - 16 * t42kP * t6 * t2 * t3 * t3 * t1P * k * t13P + 36 * t42kP * t2 * t3 * t1P * k * t1 * t1 - 2 * t42kP * t3 * t1P
         * k * k + 2 * t5 * t5 * t13kP * k * t4 + 6 * t42kP * t1P * k * t1 * t3 * t3 - 4 * t42kP * t6 * t1P * k * k * t7 * t1 + 2 * t4P * t7 * t2 * t12kP
         * k * k * t1 * t5 * t3 * t3 * t4 + 4 * t42kP * t6 * t14P * t3 * t3 * t1P * t2 * t2 + 4 * t4P * t7 * t2 * t12kP * k * k * t1 * t5 * t3 + t7 * t7 * t32kP
         * t4 * t4 * t1P * k * t2 * t2 - 8 * t42kP * t6 * t1P * k * t2 * t3 + 2 * t42kP * t7 * t7 * t2 * t1P * k + 8 * t42kP * t2 * t5 * t1P * k * k
         * t1 + 8 * t42kP * t6 * t13P * t3 * t3 * t1P * t2 * t2 - 4 * t6 * t6 * t22kP * t3 * t1P * k * k * t1 * t1 * t4 - t7 * t3P * t12kP * k * k
         * t5 * t3 - 8 * t42kP * t1P * k * t13P * t6 * t3 * t3 + 2 * t4P * t6 * t12kP * k * k * t5 * t3 * t4 * t1 + 16 * t42kP * t3 * t3 * t5 * t1P
         * k * t13P * t6 * t2 - 2 * t42kP * t2 * t5 * t5 * t1P * k * k * t3 * t3 * t1 * t1 - 16 * t42kP * t7 * t2 * t3 * t5 * t1P * k * k * t1 + 8 * t42kP
         * t2 * t5 * t1P * k * k * t3 * t1 * t1 - 36 * t42kP * t7 * t2 * t2 * t1P * k * t3 * t1 * t1 - 8 * t42kP * t7 * t7 * t1P * t13P * t2 + 2 * t5 * t5
         * t13kP * k * t2 * t3 * t3 - 4 * t42kP * t7 * t1P * t1 * t1 * t5 * t3 * t3 - 12 * t42kP * t1P * k * t1 * t6 + 8 * t42kP * t14P * t5 * t1P
         * t3 + 9 * t42kP * t2 * t2 * t5 * t5 * t1P * k * t1 * t1 - 4 * t7 * t7 * t32kP * t4 * t1P * t1 * t1 - 4 * t6 * t6 * t22kP * t14P * t1P * t3
         + 8 * t42kP * t7 * t1P * t1 * t1 * t3 + 2 * t42kP * t5 * t1P * k * k * t1 * t1 * t3 * t3 - 2 * t4P * t2 * t2 * t3 * t4 * t12kP * k * k * t5 * t1 - 36
         * t42kP * t3 * t5 * t1P * k * t1 * t1 + 2 * t42kP * t2 * t2 * t1P * k * k * t7 - 16 * t42kP * t13P * t5 * t1P * t7 * t3 + t42kP * t7
         * t7 * t2 * t2 * t1P * k * t3 * t3 - t42kP * t7 * t7 * t3 * t3 * t1P * k * k * t1 * t1 - t42kP * t6 * t6 * t1P * k * k * t1 * t1 - 2 * t42kP * t2
         * t5 * t5 * t1P * k * k * t3 * t3 - 2 * t42kP * t7 * t7 * t2 * t1P * k * k - t7 * t7 * t32kP * t1P * k * k * t4 * t4 - 18 * t42kP * t1P * k
         * t1 * t1 * t6 - 36 * t42kP * t6 * t2 * t2 * t3 * t1P * k * t1 * t1 - t42kP * t7 * t7 * t2 * t2 * t1P * k * k * t1 * t1 + 8 * t42kP * t3 * t3 * t5
         * t1P * k * t13P * t6 + 8 * t6 * t6 * t22kP * t1P * k * t13P * t4 - 8 * t42kP * t13P * t1P * t2 + 4 * t42kP * t2 * t2 * t3 * t3
         * t1P * k * t13P - 4 * t42kP * t6 * t6 * t2 * t1P * t1 * t1 * t3 * t3 - 2 * t42kP * t6 * t6 * t14P * t3 * t3 * t1P * t2 * t2 + 9 * t42kP
         * t1P * k * t1 * t1 - 8 * t7 * t7 * t32kP * t14P * t2 * t4 * t1P + 9 * t42kP * t6 * t6 * t1P * k * t1 * t1 + 4 * t42kP * t2 * t2 * t3 * t5
         * t1P * k * t6 + 4 * t42kP * t7 * t2 * t2 * t3 * t3 * t1P * t1 * t1 - 36 * t42kP * t7 * t2 * t1P * k * t1 * t1 - 12 * t42kP * t7 * t2 * t2 * t1P
         * k * t1 * t3 * t3 - 4 * t7 * t7 * t32kP * t4 * t1P * t14P + t4P * t6 * t12kP * k * k * t5 * t2 * t2 + 6 * t42kP * t2 * t2 * t5 * t5 * t1P
         * k * t1 - 8 * t42kP * t7 * t2 * t3 * t5 * t1P * k * k * t1 * t1 + t4P * t2 * t2 * t5 * t5 * t12kP * k * k + 16 * t42kP * t7 * t2 * t3 * t1P * t1
         * t1 - 2 * t7 * t7 * t32kP * t2 * t2 * t4 * t4 * t1P * t1 * t1 + 6 * t7 * t7 * t32kP * t2 * t2 * t1P * k * t1 * t4 * t4 + t4P * t2 * t2 * t5 * t5
         * t12kP * k * k * t4 + t4P * t6 * t12kP * k * k * t5 * t2 * t2 * t4 - 4 * t42kP * t2 * t2 * t5 * t5 * t1P * k * k * t1 * t3 - 4 * t42kP * t14P
         * t5 * t1P * t6 - 8 * t42kP * t2 * t3 * t5 * t1P * k - 4 * t42kP * t6 * t14P * t3 * t3 * t1P * t7 * t2 * t2 - 2 * t42kP * t1P * k * k
         * t1 * t1 * t3 - 2 * t42kP * t6 * t1P * k * k * t7 * t3 * t3 * t1 * t1 - 4 * t7 * t3P * t2 * t4 * t12kP * k * k * t5 + 18 * t42kP * t7 * t3 * t3 * t5
         * t1P * k * t1 * t1 + 8 * t42kP * t2 * t1P * k * k * t6 * t3 + 9 * t6 * t6 * t22kP * t1P * k * t1 * t1 * t4 * t4 + t4P * t6 * t12kP * k * k
         * t5 + 2 * t4P * t2 * t5 * t5 * t12kP * k * k * t4 * t1 - 16 * t42kP * t7 * t7 * t2 * t3 * t1P * t13P - 18 * t42kP * t1P * k * t1 * t1 * t7 - 4
         * t42kP * t6 * t1P * k * t2 * t2 * t3 - 2 * t42kP * t3 * t3 * t1P * k * t7 - 4 * t7 * t7 * t32kP * t1P * t1 * t1 * t2 - 2 * t42kP * t2 * t5
         * t5 * t1P * k * k - 4 * t42kP * t7 * t1P * t1 * t1 * t5 * t2 * t2 - 8 * t42kP * t6 * t2 * t1P * t1 * t1 * t5 + 8 * t42kP * t7 * t2 * t1P
         * k * t6 * t3 + 32 * t42kP * t6 * t13P * t3 * t1P * t2 + 4 * t42kP * t6 * t1P * k * t7 * t3 - 4 * t42kP * t5 * t1P * k * k * t7 * t3 + 2 * t42kP
         * t2 * t3 * t3 * t1P * k + 4 * t42kP * t7 * t2 * t1P * k * k * t1 * t1 - 8 * t42kP * t13P * t5 * t1P * t6 * t2 * t2 * t3 * t3 - 4 * t42kP
         * t6 * t2 * t3 * t3 * t5 * t1P * k * k - 12 * t42kP * t1P * k * t1 * t5 + 4 * t42kP * t6 * t1P * k * k * t1 * t3 * t3 + 2 * t4P * t6 * t12kP
         * k * k * t5 * t2 * t4 - 2 * t4P * t2 * t12kP * k * k * t5 * t3 * t3 * t1 + 4 * t42kP * t7 * t2 * t1P * k * t5 - 12 * t42kP * t6 * t2 * t2 * t3 * t3
         * t1P * k * t1 + 4 * t42kP * t2 * t1P * k * k * t6 * t3 * t3 - t7 * t3P * t2 * t2 * t3 * t12kP * k * k * t5 + 8 * t42kP * t13P * t1P * t7 - 2
         * t7 * t7 * t32kP * t1P * k * k * t1 * t1 * t2 * t4 * t4 - 4 * t42kP * t6 * t3 * t3 * t1P * t1 * t1 * t5 + 24 * t42kP * t7 * t3 * t5 * t1P * k
         * t1 - 24 * t42kP * t6 * t2 * t3 * t3 * t1P * k * t1 + 4 * t42kP * t2 * t5 * t1P * k * k * t3 * t3 - 4 * t42kP * t6 * t6 * t1P * k * k * t1 * t2
         * t3 * t3 + 12 * t6 * t6 * t22kP * t3 * t1P * k * t1 - 24 * t42kP * t1P * k * t1 * t6 * t3 + 4 * t42kP * t2 * t2 * t5 * t1P * k * k * t3 * t1
         * t1 + 2 * t42kP * t2 * t2 * t1P * k * k * t6 * t1 * t1 - 16 * t42kP * t6 * t2 * t2 * t3 * t1P * k * t13P + 9 * t42kP * t7 * t7 * t2 * t2 * t1P
         * k * t3 * t3 * t1 * t1 - 8 * t42kP * t6 * t6 * t13P * t3 * t1P - t42kP * t2 * t2 * t5 * t5 * t1P * k * k * t1 * t1 + 18 * t42kP * t6 * t1P
         * k * t1 * t1 * t7 + 2 * t42kP * t6 * t6 * t1P * k * t2 * t2 * t3 - 2 * t42kP * t7 * t7 * t2 * t2 * t1P * k * k * t1 * t1 * t3 - 4 * t42kP * t7 * t7
         * t2 * t3 * t3 * t1P * t1 * t1 + 18 * t7 * t7 * t32kP * t1P * k * t1 * t1 * t4 + 8 * t6 * t6 * t22kP * t3 * t1P * k * t13P - 2 * t6 * t6 * t22kP
         * t3 * t4 * t4 * t1P * k * k + 8 * t42kP * t7 * t2 * t2 * t3 * t1P * t1 * t1 - 2 * t42kP * t7 * t7 * t2 * t2 * t3 * t3 * t1P * t14P - 4 * t42kP
         * t5 * t1P * k * k * t1 * t7 * t3 * t3 - 4 * t42kP * t7 * t1P * t1 * t1 * t5 - 4 * t42kP * t6 * t6 * t14P * t3 * t1P * t2 * t2 + 6 * t42kP
         * t7 * t7 * t2 * t2 * t1P * k * t1 - 8 * t6 * t6 * t22kP * t13P * t1P * t4 + 2 * t42kP * t2 * t2 * t1P * k * k * t6 * t3 * t3 * t1 * t1 - 4 * t42kP
         * t7 * t7 * t2 * t3 * t3 * t1P * t14P - 2 * t42kP * t6 * t6 * t2 * t2 * t1P * t1 * t1 - t42kP * t6 * t6 * t1P * k * k * t2 * t2 * t3 * t3 + 9
         * t42kP * t7 * t7 * t1P * k * t1 * t1 - 16 * t42kP * t13P * t5 * t1P * t7 * t2 - t7 * t3P * t12kP * k * k * t1 * t5 * t4 * t4 - t7 * t7
         * t32kP * t1P * k * k * t2 * t2 - 2 * t42kP * t2 * t2 * t3 * t3 * t5 * t1P * k + 2 * t4P * t7 * t2 * t2 * t12kP * k * k * t1 * t5 * t3 - 32 * t42kP
         * t7 * t13P * t2 * t3 * t5 * t1P - 16 * t42kP * t1P * k * t13P * t6 * t3 - 2 * t6 * t6 * t22kP * t3 * t1P * k * k * t1 * t1 - 4 * t7 * t7
         * t32kP * t1P * k * k * t1 * t1 * t2 * t4 + 16 * t42kP * t2 * t5 * t1P * k * t13P * t6 - 32 * t42kP * t7 * t2 * t1P * k * t13P * t3
         + t4P * t5 * t5 * t12kP * k * k * t1 * t4 + 8 * t42kP * t6 * t3 * t3 * t1P * k * t13P * t7 + 16 * t7 * t7 * t32kP * t1P * k * t13P * t2
         * t4 - 24 * t42kP * t1P * k * t1 * t7 * t3 + 2 * t4P * t7 * t2 * t2 * t12kP * k * k * t1 * t5 * t3 * t4 + 6 * t42kP * t6 * t6 * t1P * k * t1 * t2
         * t2 + 6 * t42kP * t7 * t7 * t2 * t2 * t1P * k * t1 * t3 * t3 + 18 * t42kP * t6 * t6 * t1P * k * t1 * t1 * t2 + 8 * t42kP * t2 * t1P * k * k * t6
         * t1 * t3 * t3 - 12 * t4P * t4 * t5 * t1P * k * t1 * t7 * t3P - 18 * t4P * t5 * t1P * k * t1 * t1 * t6 * t2P + 2 * t4P * t2 * t2 * t3 * t1P
         * k * t7 * t3P + 8 * t4P * t6 * t6 * t2 * t1P * t13P * t2P - 36 * t4P * t4 * t5 * t1P * k * t1 * t1 * t6 * t2P * t3 - 36 * t4P * t4
         * t5 * t1P * k * t1 * t1 * t7 * t3P * t2 + 4 * t6 * t2P * t2 * t1P * t1 * t1 * t4P * t5 * t3 * t3 + 8 * t4P * t7 * t7 * t14P * t2 * t4 * t1P
         * t3P * t3 + 12 * t7 * t3P * t2 * t1P * k * t1 * t6 * t2P + 12 * t4P * t3 * t3 * t4 * t1P * k * t6 * t2P * t1 * t2 + 8 * t4P * t3 * t3 * t4
         * t1P * k * t6 * t2P * t13P * t2 - 18 * t4P * t6 * t6 * t4 * t1P * k * t1 * t1 * t2P * t3 * t3 * t2 - 18 * t6 * t2P * t3 * t3 * t5 * t1P
         * k * t1 * t1 * t4P + 16 * t4P * t6 * t2 * t1P * t13P * t7 * t3P * t4 + 4 * t4P * t6 * t2 * t2 * t1P * t1 * t1 * t7 * t3P * t4 - 12 * t6
         * t2P * t5 * t12kP * t2 * t4 * t1 * t1 * t3 - 3 * t6 * t2P * t5 * t12kP * t2 * t4 * t4 * t1 * t1 - 6 * t6 * t2P * t5 * t12kP * t2 * t4 * t4
         * t1 * t1 * t3 - 12 * t6 * t2P * t5 * t12kP * t2 * t4 * t13P * t3 - 4 * t5 * t5 * t12kP * t2 * t4 * k * t4P * t3 * t3 - 4 * t5 * t12kP * t2
         * t4 * k * t4P * t6 * t3 * t3 + 2 * t5 * t12kP * t2 * t2 * t4 * t4 * k * t7 * t3P * t3 - 2 * t5 * t12kP * t2 * t2 * t4 * k * t4P * t6 * t3 * t3 + 2
         * t5 * t12kP * t2 * t4 * t4 * k * t6 * t2P * t3 * t3 + 2 * t7 * t3P * t1P * k * k * t1 * t1 * t4P * t6 * t3 + 4 * t4P * t14P * t5 * t1P
         * t6 * t2P * t4 + 8 * t4P * t14P * t5 * t1P * t6 * t2P * t3 + 16 * t4P * t13P * t5 * t1P * t6 * t2P * t3 * t4 + 4 * t4P * t14P
         * t5 * t1P * t7 * t3P * t4 + 8 * t4P * t14P * t5 * t1P * t7 * t3P * t2 + 8 * t4P * t14P * t5 * t1P * t6 * t2P * t3 * t4 + 6 * t5
         * t12kP * t2 * t3 * t3 * k * t1 * t1 * t6 * t2P * t4 * t4 - 6 * t5 * t12kP * t2 * t2 * t3 * t3 * k * t1 * t1 * t4P * t6 - 2 * t4P * t2 * t1P * k
         * k * t6 * t2P * t3 * t3 + 8 * t4P * t6 * t2 * t2 * t1P * t13P * t7 * t3P + 16 * t5 * t12kP * t3 * t4 * k * t1 * t4P + 16 * t5 * t12kP
         * t3 * t3 * t4 * k * t1 * t6 * t2P * t2 - 2 * t6 * t6 * t2P * t2 * t3 * t3 * t1P * k * t4P * t4 - 8 * t6 * t6 * t2P * t3 * t3 * t1P * k * t13P
         * t4P + 12 * t5 * t12kP * t2 * t3 * t3 * k * t1 * t1 * t6 * t2P * t4 + 3 * t4P * t6 * t5 * t12kP * t4 * t1 * t1 + 6 * t4P * t6 * t5 * t12kP
         * t4 * t1 * t1 * t3 + 6 * t4P * t6 * t5 * t12kP * t4 * t1 * t1 * t2 + 12 * t4P * t6 * t5 * t12kP * t4 * t1 * t1 * t2 * t3 + 4 * t4P * t6 * t6 * t1P
         * k * k * t2P * t1 + 4 * t4P * t6 * t6 * t1P * k * k * t2P * t3 + 2 * t4P * t6 * t6 * t1P * k * k * t2P * t4 + 2 * t4P * t6 * t6 * t1P * k
         * k * t2P + 6 * t5 * t12kP * t13P * t4 * t4P * t7 * t2 + 4 * t4P * t14P * t5 * t1P * t6 * t2P * t2 + 8 * t4P * t13P * t5 * t1P
         * t6 * t2P * t2 + 8 * t4P * t13P * t5 * t1P * t7 * t3P * t2 * t2 + 16 * t4P * t13P * t5 * t1P * t6 * t2P * t3 * t2 - 3 * t7 * t3P
         * t5 * t12kP * t2 * t2 * t13P + 16 * t5 * t12kP * t2 * k * t1 * t6 * t2P * t3 + 2 * t4P * t4 * t1P * k * t6 * t2P * t2 + 8 * t4P * t5
         * t12kP * k * t1 - 2 * t6 * t2P * t4 * t1P * k * t4P * t7 - 2 * t7 * t3P * t1P * k * k * t4P * t3 + 36 * t4P * t3 * t4 * t1P * k * t6
         * t2P * t1 * t1 * t2 - 18 * t6 * t2P * t1P * k * t1 * t1 * t4P * t7 - 18 * t6 * t6 * t2P * t1P * k * t1 * t1 * t4P - 2 * t4P * t1P * k
         * k * t1 * t1 * t6 * t2P - 4 * t4P * t1P * k * k * t1 * t6 * t2P * t4 - 4 * t4P * t1P * k * k * t1 * t6 * t2P - 2 * t4P * t7 * t7 * t1P
         * k * t3P + 8 * t4P * t6 * t2 * t2 * t1P * t13P * t7 * t3P * t4 - 6 * t6 * t2P * t5 * t12kP * t3 * t4 * t4 * t1 * t1 - 3 * t6 * t2P * t5
         * t12kP * t3 * t3 * t4 * t4 * t1 * t1 - 8 * t4P * t13P * t1P * t7 * t3P * t2 * t2 - 18 * t4P * t5 * t1P * k * t1 * t1 * t7 * t3P + 8 * t5
         * t12kP * k * t1 * t4P * t3 * t3 + 8 * t4P * t6 * t2 * t1P * t14P * t7 * t3P * t4 - 4 * t4P * t4 * t1P * t1 * t1 * t6 * t2P * t3 * t3
         * t2 + 4 * t4P * t6 * t6 * t14P * t3 * t3 * t1P * t2P * t2 + 18 * t4P * t2 * t3 * t3 * t1P * k * t1 * t1 * t6 * t2P + 24 * t4P * t2 * t3
         * t1P * k * t1 * t6 * t2P + 4 * t7 * t7 * t3P * t1P * k * k * t1 * t4P * t3 * t4 - 6 * t5 * t12kP * t2 * t2 * t3 * t3 * k * t1 * t1 * t4P * t7 - 24
         * t4P * t7 * t7 * t2 * t1P * k * t3P * t1 * t4 * t3 + 4 * t5 * t5 * t13kP * k * t2 * t1 * t3 * t3 + 8 * t5 * t5 * t13kP * k * t2 * t1 * t3 * t3 * t4
         + 2 * t5 * t12kP * k * t7 * t3P * t2 * t2 * t3 - 8 * t7 * t7 * t3P * t1P * k * t13P * t4P * t3 * t4 + 12 * t4P * t1P * k * t1 * t7 * t3P
         * t3 + 12 * t4P * t1P * k * t1 * t6 * t2P * t3 * t3 - 4 * t6 * t2P * t14P * t1P * t7 * t3P * t4 * t4 + 8 * t5 * t12kP * k * t1 * t6 * t2P
         * t3 * t3 - 16 * t5 * t12kP * k * t1 * t4P * t7 * t3 + 2 * t6 * t2P * t4 * t4 * t1P * k * t7 * t3P * t3 + 4 * t6 * t2P * t4 * t1P * k * t7 * t3P
         * t3 - 2 * t5 * t12kP * t2 * t2 * t4 * k * t4P * t7 - 16 * t5 * t12kP * t2 * t4 * k * t4P * t6 * t1 - 8 * t5 * t12kP * t2 * t4 * k * t4P * t6
         * t3 - 2 * t5 * t5 * t12kP * t2 * t2 * t4 * k * t4P - 8 * t5 * t5 * t12kP * t2 * t4 * k * t4P * t3 - 8 * t5 * t12kP * t2 * t4 * k * t4P * t7 * t3
         + 4 * t5 * t12kP * t2 * t4 * k * t4P * t3 * t3 + 4 * t5 * t12kP * t2 * t4 * k * t4P + 2 * t4P * t6 * t6 * t1P * k * k * t2P * t3 * t3 * t2 + 4
         * t4P * t7 * t2 * t3 * t3 * t1P * t1 * t1 * t6 * t2P + 8 * t4P * t7 * t2 * t3 * t1P * t1 * t1 * t6 * t2P * t4 - 16 * t4P * t7 * t2 * t4 * t1P
         * k * t13P * t6 * t2P * t3 - 2 * t5 * t5 * t13kP * t2 * t1 * t1 * t4 * t4 - 4 * t5 * t5 * t13kP * t2 * t1 * t1 * t4 * t4 * t3 - t5 * t5 * t13kP
         * t2 * t2 * t1 * t1 * t4 * t4 - 2 * t5 * t5 * t13kP * t2 * t2 * t1 * t1 * t4 * t4 * t3 + 4 * t5 * t12kP * t2 * t4 * t4 * k * t6 * t2P * t3 - 4 * t5 * t12kP
         * t2 * t2 * t4 * k * t4P * t6 * t3 + 2 * t5 * t12kP * t2 * t2 * t4 * t4 * k * t7 * t3P - 4 * t5 * t12kP * t2 * t2 * t4 * k * t4P * t7 * t3 - 8 * t5
         * t12kP * t2 * t2 * t4 * k * t4P * t6 * t1 - 24 * t6 * t6 * t2P * t3 * t1P * k * t1 * t4P + 16 * t5 * t12kP * t3 * t3 * t4 * k * t1 * t6 * t2P
         + 16 * t5 * t12kP * t3 * t4 * t4 * k * t1 * t6 * t2P + 4 * t4P * t7 * t2 * t3 * t3 * t1P * t1 * t1 * t6 * t2P * t4 + 8 * t4P * t7 * t7 * t1P
         * t13P * t3P * t2 * t2 + 4 * t7 * t3P * t2 * t4 * t1P * k * k * t4P * t6 - 2 * t5 * t5 * t13kP * t2 * t3 * t3 * t4 * t4 * t1 * t1 - 6 * t4P
         * t7 * t5 * t12kP * k * t2 * t2 * t4 * t1 * t1 - 6 * t6 * t2P * t5 * t12kP * t2 * t4 * t13P - 3 * t6 * t2P * t5 * t12kP * t2 * t4 * t4 * t13P - 6
         * t6 * t2P * t5 * t12kP * t2 * t4 * t4 * t13P * t3 - 4 * t4P * t6 * t2 * t3 * t4 * t1P * k * t7 * t3P + 4 * t4P * t6 * t6 * t1P * k * k
         * t2P * t1 * t4 + 2 * t7 * t7 * t3P * t1P * k * k * t4P - 4 * t5 * t12kP * k * t4P * t6 * t2 - 8 * t4P * t5 * t5 * t12kP * k * t3 * t3 * t1
         + 18 * t4P * t4 * t1P * k * t6 * t2P * t1 * t1 * t2 + 12 * t5 * t12kP * t2 * t4 * k * t4P * t1 * t1 - 4 * t5 * t12kP * t2 * t4 * k * t4P * t7 - 3
         * t5 * t12kP * t13P * t7 * t3P * t3 - 4 * t6 * t6 * t2P * t2 * t3 * t1P * k * t4P * t4 - 4 * t6 * t6 * t2P * t4 * t1P * k * t4P * t3 - 4
         * t6 * t2P * t4 * t1P * k * t4P * t7 * t3 - 4 * t6 * t6 * t2P * t2 * t3 * t1P * k * t4P - 12 * t4P * t5 * t5 * t12kP * k * t3 * t1 * t1
         + 2 * t6 * t6 * t2P * t2 * t4 * t1P * k * k * t4P * t1 * t1 - 2 * t6 * t6 * t2P * t3 * t3 * t1P * k * t4P - 2 * t6 * t2P * t3 * t3 * t1P * k
         * t4P * t5 - 2 * t6 * t2P * t3 * t3 * t1P * k * t4P * t7 - 2 * t5 * t5 * t12kP * t2 * t2 * t4 * k * t4P * t3 * t3 + 8 * t7 * t7 * t3P * t13P
         * t3 * t1P * t4P * t4 - 8 * t7 * t3P * t13P * t3 * t1P * t6 * t2P * t4 * t4 - 8 * t5 * t12kP * k * t1 * t4P * t7 - 8 * t5 * t12kP
         * k * t1 * t4P * t6 - 8 * t5 * t5 * t12kP * k * t1 * t4P + 2 * t5 * t12kP * k * t4P * t2 * t2 - 24 * t6 * t2P * t3 * t1P * k * t1 * t4P
         * t7 * t4 + 16 * t5 * t12kP * t2 * k * t1 * t4P * t3 * t3 - 8 * t4P * t2 * t3 * t1P * t1 * t1 * t7 * t3P + 4 * t6 * t2P * t1P * k * k * t4P
         * t7 * t1 + 2 * t6 * t2P * t1P * k * k * t4P * t7 * t2 + 4 * t6 * t2P * t1P * k * k * t4P * t7 * t3 - 4 * t4P * t2 * t2 * t3 * t1P * t1 * t1
         * t7 * t3P - 8 * t4P * t6 * t4 * t1P * k * t13P * t7 * t3P * t3 - 12 * t4P * t7 * t7 * t1P * k * t1 * t3P + 4 * t6 * t2P * t14P
         * t1P * t4P * t7 * t2 - 8 * t6 * t2P * t14P * t1P * t4P * t2 * t3 - 8 * t6 * t2P * t14P * t1P * t7 * t3P * t4 - 4 * t6 * t2P
         * t14P * t1P * t7 * t3P * t2 - 16 * t6 * t2P * t13P * t1P * t7 * t3P * t2 * t4 - 2 * t4P * t5 * t1P * k * t7 * t3P * t2 * t2 + 16
         * t4P * t6 * t2 * t1P * t13P * t7 * t3P - 4 * t7 * t3P * t1P * t1 * t1 * t6 * t2P - 4 * t7 * t3P * t1P * t1 * t1 * t4P * t3 + 4
         * t7 * t3P * t1P * t1 * t1 * t4P * t5 * t2 * t2 * t3 + 4 * t7 * t3P * t1P * t1 * t1 * t4P * t5 * t2 * t2 + 8 * t7 * t3P * t1P * t1 * t1
         * t4P * t5 * t2 + 4 * t7 * t3P * t1P * t1 * t1 * t4P * t5 * t3 + 2 * t4P * t2 * t5 * t1P * k * k * t6 * t2P + 4 * t4P * t2 * t5 * t1P
         * k * k * t7 * t3P - 6 * t4P * t5 * t5 * t12kP * k * t3 * t3 * t1 * t1 + 8 * t4P * t3 * t3 * t4 * t1P * k * t6 * t2P * t13P + 4 * t6 * t2P
         * t3 * t1P * k * t4P + 12 * t7 * t3P * t2 * t2 * t1P * k * t1 * t4P * t3 + 4 * t7 * t7 * t3P * t2 * t3 * t1P * k * k * t4P + 12 * t4P
         * t1P * k * t1 * t6 * t2P * t2 + 24 * t4P * t3 * t4 * t1P * k * t6 * t2P * t1 + 2 * t4P * t3 * t3 * t4 * t1P * k * t6 * t2P - 3 * t7 * t3P
         * t5 * t12kP * t4 * t4 * t1 * t1 - 6 * t7 * t3P * t5 * t12kP * t4 * t4 * t1 * t1 * t2 - 2 * t6 * t2P * t4 * t1P * k * t4P * t7 * t3 * t3 - 6 * t5
         * t12kP * t1 * t1 * t4P * t2 * t3 * t3 * t4 + 3 * t5 * t12kP * t1 * t1 * t4P * t7 * t3 * t3 * t4 - 3 * t5 * t12kP * t1 * t1 * t6 * t2P * t4
         * t4 + 3 * t5 * t5 * t12kP * t1 * t1 * t4P * t4 + 3 * t5 * t12kP * t1 * t1 * t4P * t6 * t2 * t2 * t3 * t3 * t4 - 2 * t6 * t6 * t2P * t2 * t1P
         * k * t4P + 2 * t7 * t7 * t3P * t1P * k * k * t4P * t2 * t2 + 2 * t7 * t3P * t1P * k * k * t4P * t6 * t2 * t2 - 2 * t7 * t3P * t1P * k * k
         * t6 * t2P * t2 + 6 * t4P * t5 * t12kP * k * t1 * t1 - 4 * t5 * t5 * t12kP * t2 * t2 * t4 * k * t4P * t3 - 8 * t6 * t2P * t2 * t1P * t1 * t1
         * t7 * t3P * t4 - 4 * t6 * t2P * t2 * t1P * t1 * t1 * t7 * t3P * t4 * t4 * t3 - 8 * t6 * t2P * t2 * t1P * t1 * t1 * t7 * t3P * t4 * t3 - 4 * t6
         * t2P * t2 * t1P * t1 * t1 * t7 * t3P * t3 + 8 * t6 * t2P * t2 * t1P * t1 * t1 * t4P * t5 * t3 - 4 * t6 * t2P * t2 * t1P * t1 * t1 * t4P
         * t3 * t3 - 4 * t6 * t2P * t2 * t1P * t1 * t1 * t4P + 4 * t4P * t6 * t2 * t2 * t1P * t1 * t1 * t7 * t3P + 8 * t4P * t6 * t2 * t1P * t1 * t1
         * t7 * t3P * t4 + 8 * t7 * t3P * t1P * k * t13P * t6 * t2P * t3 * t4 * t4 * t2 + 16 * t7 * t3P * t1P * k * t13P * t6 * t2P * t4 + 18
         * t7 * t3P * t1P * k * t1 * t1 * t4P * t2 * t2 + 18 * t7 * t3P * t1P * k * t1 * t1 * t6 * t2P * t3 * t4 * t4 + 8 * t7 * t3P * t1P * k * t13P
         * t6 * t2P * t3 + 36 * t7 * t3P * t1P * k * t1 * t1 * t6 * t2P * t3 * t4 + 8 * t7 * t3P * t1P * k * t13P * t6 * t2P * t2 + 18 * t7 * t3P
         * t1P * k * t1 * t1 * t4P * t2 * t2 * t3 * t4 + 16 * t7 * t3P * t1P * k * t13P * t4P * t2 * t3 * t4 + 2 * t7 * t3P * t1P * k * k * t1 * t1
         * t4P * t5 * t4 + 8 * t5 * t12kP * k * t4P * t2 * t3 + 4 * t4P * t7 * t7 * t2 * t2 * t3 * t1P * t1 * t1 * t3P + 8 * t4P * t7 * t7 * t2 * t3
         * t1P * t1 * t1 * t3P * t4 - t5 * t5 * t13kP * t2 * t2 * t3 * t3 * t4 * t4 * t1 * t1 - 2 * t6 * t2P * t1P * k * k * t4P - 2 * t6 * t2P * t1P
         * k * t4P * t7 + 8 * t7 * t3P * t1P * t1 * t1 * t4P * t5 * t2 * t3 - 6 * t5 * t12kP * k * t1 * t1 * t4P * t7 - 6 * t5 * t5 * t12kP * k * t1
         * t1 * t4P - 8 * t7 * t3P * t1P * k * k * t1 * t4P * t2 * t4 - 16 * t7 * t3P * t13P * t3 * t1P * t4P * t2 * t4 + 4 * t7 * t3P * t2 * t3
         * t1P * k * k * t4P * t6 + 2 * t4P * t3 * t1P * k * t7 * t3P + 18 * t4P * t1P * k * t1 * t1 * t6 * t2P * t2 + 2 * t6 * t2P * t2 * t1P
         * k * t7 * t3P + 8 * t4P * t6 * t2 * t1P * t1 * t1 * t7 * t3P * t4 * t3 + 4 * t4P * t6 * t2 * t2 * t1P * t1 * t1 * t7 * t3P * t3 + 12 * t7 * t3P
         * t2 * t2 * t1P * k * t1 * t4P - 12 * t6 * t2P * t1P * k * t1 * t4P * t7 + 2 * t6 * t6 * t2P * t3 * t3 * t1P * k * k * t1 * t1 * t4P * t2
         * t4 + 4 * t4P * t7 * t7 * t14P * t2 * t2 * t4 * t1P * t3P + 8 * t4P * t7 * t14P * t2 * t4 * t1P * t6 * t2P * t3 - 16 * t4P * t6 * t6
         * t4 * t1P * k * t13P * t2P * t3 - 4 * t6 * t2P * t3 * t1P * k * k * t1 * t7 * t3P - 36 * t4P * t7 * t2 * t4 * t1P * k * t1 * t1 * t6 * t2P
         * t3 + 4 * t4P * t7 * t14P * t2 * t4 * t1P * t6 * t2P * t3 * t3 - 8 * t4P * t7 * t2 * t4 * t1P * k * t13P * t6 * t2P - 4 * t5 * t5 * t12kP
         * k * t4P * t3 - 4 * t5 * t5 * t12kP * k * t4P * t2 - 2 * t5 * t5 * t12kP * k * t4P * t2 * t2 - 2 * t5 * t5 * t12kP * k * t4P * t3 * t3 + 4
         * t5 * t12kP * k * t4P * t2 - 8 * t6 * t2P * t3 * t1P * k * k * t1 * t4P * t2 - 4 * t6 * t2P * t3 * t1P * k * k * t1 * t1 * t4P + 6 * t5 * t12kP
         * t2 * t2 * t3 * t3 * k * t1 * t1 * t4P * t4 + 2 * t5 * t12kP * k * t4P * t4 + 3 * t5 * t5 * t12kP * t13P * t4P * t2 * t2 * t3 * t3 - 3 * t5 * t12kP
         * t13P * t6 * t2P * t3 * t3 + 3 * t5 * t12kP * t13P * t4P * t7 * t2 * t2 - 8 * t6 * t2P * t3 * t1P * k * k * t1 * t7 * t3P * t4 + 4 * t5
         * t12kP * k * t4P * t3 - 4 * t6 * t2P * t3 * t1P * k * k * t1 * t7 * t3P * t2 - 2 * t6 * t2P * t3 * t1P * k * k * t1 * t1 * t7 * t3P + 2 * t5
         * t12kP * k * t7 * t3P + 2 * t5 * t12kP * k * t6 * t2P - 4 * t6 * t2P * t3 * t3 * t1P * k * k * t1 * t4P * t2 - 4 * t6 * t2P * t3 * t1P
         * k * k * t1 * t1 * t4P * t2 - 2 * t6 * t2P * t3 * t3 * t1P * k * k * t1 * t1 * t4P - 36 * t4P * t7 * t7 * t2 * t4 * t1P * k * t1 * t1 * t3P - 2
         * t5 * t5 * t12kP * k * t4P * t4 + 8 * t7 * t3P * t5 * t12kP * k * t2 * t2 * t4 * t4 * t1 + 4 * t4P * t14P * t5 * t1P * t6 * t2P + 16
         * t4P * t13P * t5 * t1P * t6 * t2P * t3 + 8 * t4P * t13P * t5 * t1P * t6 * t2P * t4 + 8 * t4P * t13P * t5 * t1P * t6 * t2P
         + 8 * t4P * t13P * t5 * t1P * t7 * t3P - 8 * t5 * t12kP * t3 * t3 * t4 * k * t1 * t4P * t6 * t2 * t2 - 18 * t4P * t7 * t2 * t4 * t1P * k
         * t1 * t1 * t6 * t2P - 2 * t5 * t12kP * k * t4P * t6 - 2 * t4P * t2 * t3 * t3 * t5 * t1P * k * t6 * t2P - 4 * t4P * t2 * t3 * t5 * t1P * k
         * t6 * t2P * t4 - 4 * t4P * t2 * t3 * t5 * t1P * k * t6 * t2P - 4 * t4P * t2 * t3 * t5 * t1P * k * t7 * t3P - 4 * t6 * t2P * t3 * t3 * t1P
         * k * k * t1 * t4P + 8 * t6 * t6 * t2P * t3 * t1P * k * k * t1 * t4P * t2 - 3 * t4P * t5 * t12kP * t2 * t2 * t3 * t3 * t13P - 16 * t4P * t7
         * t7 * t2 * t4 * t1P * k * t13P * t3P * t3 - 4 * t6 * t2P * t3 * t1P * k * k * t1 * t1 * t4P * t2 * t4 + 8 * t6 * t2P * t3 * t1P * k * k * t1
         * t4P * t5 * t4 - 18 * t4P * t7 * t7 * t2 * t2 * t4 * t1P * k * t1 * t1 * t3P * t3 - 16 * t4P * t6 * t5 * t12kP * k * t2 * t3 * t3 * t1 + 4 * t6
         * t2P * t3 * t3 * t1P * k * k * t1 * t4P * t5 * t4 - 2 * t4P * t6 * t1P * k * t7 * t3P * t2 * t2 + 8 * t6 * t2P * t3 * t1P * k * k * t1 * t4P
         * t7 * t4 + 8 * t6 * t6 * t2P * t3 * t1P * k * k * t1 * t4P * t4 - 4 * t5 * t12kP * k * t4P * t6 * t3 - 2 * t6 * t2P * t3 * t1P * k * k * t1 * t1
         * t7 * t3P * t2 * t4 * t4 - 4 * t5 * t12kP * k * t4P * t7 * t2 + 12 * t5 * t12kP * t2 * t2 * t3 * k * t1 * t1 * t4P * t4 + 4 * t4P * t7 * t7 * t1P
         * t14P * t3P + 4 * t4P * t7 * t7 * t1P * t14P * t3P * t2 * t2 + 4 * t6 * t6 * t2P * t3 * t1P * k * k * t1 * t1 * t4P * t2 * t4 - 4 * t4P
         * t6 * t1P * k * t7 * t3P * t2 * t3 - 2 * t4P * t6 * t1P * k * t7 * t3P * t4 * t3 + 2 * t5 * t12kP * k * t6 * t2P * t2 - 2 * t4P * t6 * t1P
         * k * t7 * t3P * t2 * t2 * t4 - 2 * t5 * t12kP * k * t4P * t6 * t2 * t2 - 12 * t5 * t12kP * t2 * t3 * t3 * k * t1 * t1 * t4P * t7 * t4 + 4 * t6 * t2P
         * t3 * t3 * t1P * k * k * t1 * t4P * t7 * t2 * t4 - 4 * t4P * t2 * t2 * t3 * t4 * t1P * k * k * t7 * t3P * t1 + 4 * t5 * t12kP * k * t4P * t2
         * t2 * t3 + 2 * t5 * t12kP * k * t7 * t3P * t2 * t2 - 2 * t5 * t12kP * k * t4P * t7 * t2 * t2 - 6 * t4P * t5 * t12kP * t2 * t2 * t3 * t13P
         + 2 * t5 * t12kP * k * t6 * t2P * t3 * t3 + 2 * t5 * t12kP * k * t7 * t3P * t3 - 2 * t4P * t6 * t1P * k * t7 * t3P * t2 * t2 * t3 + 4 * t5 * t12kP
         * k * t4P * t2 * t3 * t3 - 18 * t4P * t4 * t5 * t1P * k * t1 * t1 * t7 * t3P - 24 * t4P * t4 * t5 * t1P * k * t1 * t7 * t3P * t2 + 2 * t5 * t12kP
         * k * t4P * t2 * t2 * t3 * t3 - 8 * t6 * t2P * t3 * t1P * k * k * t1 * t4P * t4 - 2 * t5 * t12kP * k * t4P * t6 * t3 * t3 - 2 * t5 * t12kP * k
         * t4P * t7 * t3 * t3 - 2 * t6 * t2P * t3 * t3 * t1P * k * k * t1 * t1 * t4P * t2 * t4 - 24 * t4P * t3 * t5 * t1P * k * t1 * t7 * t3P * t2 + 4
         * t6 * t2P * t3 * t1P * k * k * t1 * t1 * t4P * t7 * t2 * t4 - 8 * t6 * t2P * t3 * t1P * k * k * t1 * t4P + 4 * t4P * t6 * t1P * t14P
         * t7 * t3P * t4 - 4 * t6 * t2P * t3 * t1P * k * k * t1 * t1 * t4P * t4 - 4 * t5 * t5 * t12kP * k * t4P * t3 * t4 - 18 * t4P * t7 * t7 * t2 * t2
         * t4 * t1P * k * t1 * t1 * t3P + 4 * t6 * t2P * t3 * t3 * t1P * k * k * t1 * t4P * t7 + 4 * t6 * t2P * t3 * t1P * k * k * t1 * t1 * t4P * t5
         + 4 * t5 * t12kP * k * t4P * t3 * t4 + 2 * t5 * t12kP * k * t7 * t3P * t4 * t4 + 4 * t6 * t2P * t3 * t3 * t1P * k * k * t1 * t4P * t5 + 4 * t6
         * t6 * t2P * t3 * t1P * k * k * t1 * t1 * t4P + 4 * t6 * t6 * t2P * t3 * t3 * t1P * k * k * t1 * t4P + 8 * t6 * t2P * t3 * t1P * k * k * t1
         * t4P * t7 * t2 + 4 * t6 * t2P * t3 * t1P * k * k * t1 * t1 * t4P * t7 + 4 * t5 * t12kP * k * t7 * t3P * t4 + 4 * t5 * t12kP * k * t7 * t3P
         * t2 - 16 * t4P * t7 * t7 * t2 * t4 * t1P * k * t13P * t3P - 18 * t4P * t7 * t2 * t4 * t1P * k * t1 * t1 * t6 * t2P * t3 * t3 - 4 * t6 * t2P
         * t3 * t3 * t1P * k * k * t1 * t4P * t4 - 6 * t4P * t5 * t12kP * t2 * t3 * t3 * t1 * t1 - 4 * t5 * t5 * t12kP * k * t4P * t2 * t2 * t3 - 4 * t6
         * t2P * t3 * t1P * k * k * t1 * t7 * t3P * t4 * t4 - 8 * t4P * t6 * t5 * t12kP * k * t2 * t2 * t3 * t3 * t1 - 6 * t4P * t5 * t12kP * t2 * t2
         * t3 * t1 * t1 + 16 * t5 * t12kP * t3 * t4 * t4 * k * t1 * t6 * t2P * t2 - 8 * t4P * t4 * t5 * t1P * k * t13P * t6 * t2P * t3 * t3 * t2 + 8 * t5
         * t12kP * t3 * t3 * t4 * t4 * k * t1 * t6 * t2P * t2 - 3 * t4P * t5 * t12kP * t2 * t2 * t3 * t3 * t1 * t1 - 12 * t4P * t5 * t12kP * t2 * t3
         * t13P - 6 * t4P * t5 * t12kP * t2 * t3 * t3 * t13P + 4 * t6 * t2P * t3 * t1P * k * k * t1 * t1 * t4P * t7 * t4 - 2 * t4P * t2 * t2 * t3
         * t4 * t1P * k * k * t7 * t3P * t1 * t1 - 36 * t4P * t7 * t7 * t2 * t4 * t1P * k * t1 * t1 * t3P * t3 - 4 * t5 * t12kP * k * t4P * t7 * t3 - 36
         * t4P * t2 * t5 * t1P * k * t1 * t1 * t7 * t3P - 12 * t4P * t2 * t2 * t5 * t1P * k * t1 * t7 * t3P - 4 * t4P * t6 * t1P * k * t7 * t3P
         * t2 * t4 - 4 * t6 * t2P * t3 * t1P * k * k * t1 * t7 * t3P * t2 * t4 * t4 + 4 * t6 * t6 * t2P * t3 * t3 * t1P * k * k * t1 * t4P * t2 * t4 + 8 * t6
         * t6 * t2P * t3 * t1P * k * k * t1 * t4P * t2 * t4 + 4 * t6 * t6 * t2P * t3 * t1P * k * k * t1 * t1 * t4P * t4 - 2 * t5 * t5 * t12kP * k * t4P
         * t2 * t2 * t3 * t3 - 8 * t5 * t5 * t12kP * k * t4P * t2 * t3 + 4 * t4P * t6 * t6 * t2 * t1P * t1 * t1 * t2P * t3 * t3 + 8 * t6 * t2P * t3 * t1P
         * k * k * t1 * t4P * t7 * t2 * t4 + 4 * t6 * t6 * t2P * t3 * t3 * t1P * k * k * t1 * t4P * t4 + 8 * t4P * t6 * t2 * t1P * t1 * t1 * t7 * t3P * t3 - 2
         * t6 * t6 * t2P * t4 * t1P * k * t4P - 2 * t5 * t12kP * k * t4P * t7 - 2 * t4P * t6 * t2 * t2 * t3 * t4 * t1P * k * t7 * t3P + 2 * t6 * t2P
         * t1P * k * k * t4P * t7 * t1 * t1 - 18 * t4P * t6 * t4 * t1P * k * t1 * t1 * t7 * t3P * t3 + 4 * t5 * t5 * t13kP * k * t3 * t3 * t4 * t1 - 4 * t7
         * t3P * t1P * t1 * t1 * t4P + 2 * t6 * t2P * t1P * k * t7 * t3P + 2 * t5 * t5 * t13kP * k * t1 * t4 * t4 + 3 * t5 * t5 * t12kP * t13P
         * t4P - 2 * t5 * t5 * t13kP * t3 * t1 * t1 - t5 * t5 * t13kP * t3 * t3 * t1 * t1 + 4 * t6 * t2P * t2 * t1P * t1 * t1 * t4P * t7 + 4 * t6 * t2P
         * t2 * t1P * t1 * t1 * t4P * t5 - 12 * t7 * t7 * t3P * t4 * t1P * k * t1 * t4P * t3 + 4 * t7 * t3P * t1P * t1 * t1 * t4P * t5 - 4 * t7 * t3P
         * t1P * t1 * t1 * t6 * t2P * t3 - 4 * t7 * t3P * t4 * t4 * t1P * t1 * t1 * t6 * t2P * t3 - 8 * t4P * t2 * t1P * t1 * t1 * t7 * t3P - 4 * t4P
         * t2 * t2 * t1P * t1 * t1 * t7 * t3P - 8 * t6 * t2P * t14P * t1P * t4P * t2 * t3 * t4 - 8 * t6 * t2P * t13P * t1P * t4P * t4 + 4
         * t6 * t2P * t14P * t1P * t4P * t7 * t2 * t3 * t3 + 8 * t7 * t3P * t1P * k * k * t1 * t4P * t6 * t2 - 4 * t7 * t3P * t1P * k * k * t1
         * t4P * t3 + 4 * t4P * t6 * t1P * t1 * t1 * t7 * t3P * t4 + 16 * t4P * t5 * t12kP * k * t1 * t3 - 8 * t7 * t3P * t1P * k * t13P * t4P
         * t6 * t2 * t2 + 8 * t7 * t3P * t1P * k * t13P * t6 * t2P * t3 * t4 * t4 - 8 * t7 * t3P * t14P * t3 * t1P * t6 * t2P * t4 - 8 * t7 * t3P
         * t13P * t3 * t1P * t4P * t2 * t2 - 4 * t5 * t12kP * k * t4P * t7 * t2 * t2 * t3 - 8 * t4P * t13P * t1P * t7 * t3P + 24 * t6 * t2P
         * t3 * t1P * k * t1 * t7 * t3P * t4 - 18 * t4P * t4 * t5 * t1P * k * t1 * t1 * t7 * t3P * t2 * t2 - 24 * t7 * t3P * t2 * t1P * k * t1 * t4P
         * t6 + 4 * t7 * t3P * t4 * t5 * t1P * k * k * t1 * t1 * t4P * t2 + 8 * t7 * t3P * t4 * t5 * t1P * k * k * t1 * t4P * t2 * t3 + 8 * t7 * t3P * t4
         * t5 * t1P * k * k * t1 * t4P * t2 + 6 * t5 * t12kP * t3 * t3 * k * t1 * t1 * t6 * t2P - 6 * t5 * t12kP * t1 * t1 * t7 * t3P * t4 - 3 * t5 * t12kP
         * t1 * t1 * t7 * t3P * t4 * t4 * t3 - 6 * t5 * t12kP * t1 * t1 * t4P * t2 - 6 * t5 * t12kP * t1 * t1 * t7 * t3P * t4 * t3 - 6 * t5 * t12kP * t1
         * t1 * t7 * t3P * t2 * t3 - 3 * t5 * t12kP * t1 * t1 * t7 * t3P * t3 + 3 * t5 * t5 * t12kP * t1 * t1 * t4P * t2 * t2 * t3 * t3 - 3 * t5 * t12kP
         * t1 * t1 * t6 * t2P * t3 * t3 + 3 * t5 * t12kP * t1 * t1 * t4P * t6 * t2 * t2 - 3 * t5 * t12kP * t1 * t1 * t6 * t2P * t2 - 32 * t4P * t6 * t5
         * t12kP * k * t2 * t1 * t3 - 8 * t4P * t6 * t5 * t12kP * k * t2 * t2 * t1 - 16 * t4P * t6 * t5 * t12kP * k * t2 * t2 * t1 * t3 - 16 * t5 * t12kP
         * t3 * t4 * k * t1 * t4P * t7 - 36 * t6 * t2P * t3 * t1P * k * t1 * t1 * t4P * t7 + 4 * t4P * t2 * t2 * t5 * t1P * k * k * t7 * t3P * t1 * t3
         + 2 * t4P * t2 * t2 * t5 * t1P * k * k * t7 * t3P * t4 * t3 + 8 * t4P * t7 * t7 * t1P * t13P * t3P + 8 * t4P * t7 * t7 * t1P * t1 * t1
         * t3P * t2 - 4 * t42kP * t2 * t5 * t5 * t1P * k * k * t1 * t3 * t3 + t42kP * t6 * t6 * t1P * k * t2 * t2 * t3 * t3 + 8 * t42kP * t2 * t3 * t3
         * t1P * k * t13P - t6 * t2P * t2 * t12kP * k * k * t1 * t5 - 2 * t42kP * t7 * t7 * t3 * t3 * t1P * k * k * t1 - 2 * t42kP * t7 * t7 * t2 * t1P
         * k * k * t1 * t1 * t3 * t3 - 4 * t42kP * t7 * t2 * t2 * t1P * k * k * t1 * t6 - 48 * t42kP * t3 * t5 * t1P * k * t1 * t2 - 2 * t42kP * t7 * t7 * t2
         * t1P * k * k * t1 * t1 - t6 * t2P * t3 * t3 * t12kP * k * k * t1 * t5 * t2 - 36 * t42kP * t1P * k * t1 * t1 * t6 * t3 - 4 * t42kP * t7 * t7 * t2
         * t1P * k * k * t1 + t4P * t5 * t5 * t12kP * k * k * t4 - t6 * t2P * t3 * t3 * t12kP * k * k * t1 * t5 - 16 * t42kP * t7 * t13P * t2 * t3
         * t3 * t5 * t1P + 12 * t42kP * t7 * t2 * t2 * t1P * k * t6 * t1 * t3 * t3 - t7 * t3P * t12kP * k * k * t5 * t2 * t2 - 4 * t7 * t7 * t32kP * t1P
         * t13P + 4 * t42kP * t6 * t6 * t1P * k * t2 * t3 - t4P * t2 * t2 * t12kP * k * k * t5 * t3 * t3 * t1 + 4 * t42kP * t2 * t2 * t5 * t1P * k
         * k * t1 + 4 * t4P * t7 * t2 * t3 * t4 * t12kP * k * k * t5 + 4 * t42kP * t2 * t1P * k * k * t6 * t1 * t1 - 36 * t42kP * t3 * t3 * t5 * t1P * k
         * t1 * t1 * t2 - 2 * t7 * t7 * t32kP * t1P * k * k * t1 * t1 * t2 * t2 * t4 + 9 * t42kP * t6 * t6 * t3 * t3 * t1P * k * t1 * t1 - 16 * t42kP * t6
         * t13P * t3 * t1P * t7 * t2 * t2 - 2 * t42kP * t1P * k * k * t1 * t3 * t3 + t4P * t2 * t2 * t5 * t5 * t12kP * k * k * t4 * t1 + 8 * t42kP
         * t7 * t2 * t2 * t1P * k * t6 * t13P * t3 * t3 - 8 * t42kP * t14P * t5 * t1P * t6 * t2 * t2 * t3 + 18 * t6 * t6 * t22kP * t1P * k * t1 * t1
         * t4 + 8 * t7 * t7 * t32kP * t1P * k * t13P * t2 * t2 * t4 + 8 * t42kP * t2 * t2 * t3 * t1P * t1 * t1 * t5 - 2 * t6 * t2P * t3 * t12kP * k
         * k * t1 * t5 * t2 - 4 * t42kP * t7 * t2 * t1P * k * t3 * t3 + 8 * t42kP * t6 * t6 * t2 * t2 * t3 * t1P * k * t13P - 4 * t42kP * t2 * t5 * t5
         * t1P * k * k * t3 - 2 * t42kP * t6 * t6 * t1P * k * k * t2 * t1 * t1 - 24 * t42kP * t7 * t2 * t1P * k * t1 * t3 * t3 + 4 * t42kP * t5 * t1P
         * k * k * t1 * t3 * t3 - 4 * t42kP * t2 * t2 * t5 * t1P * k * k * t7 * t3 - 4 * t6 * t6 * t22kP * t14P * t1P * t3 * t4 * t4 + 8 * t42kP * t2 * t2
         * t1P * k * k * t6 * t1 * t3 + 8 * t42kP * t3 * t3 * t5 * t1P * k * t13P * t6 * t2 * t2 + 2 * t4P * t6 * t12kP * k * k * t5 * t3 + 16 * t42kP
         * t6 * t3 * t1P * k * t13P * t7 + 2 * t42kP * t5 * t1P * k * t7 - 18 * t42kP * t1P * k * t1 * t1 * t6 * t3 * t3 + t4P * t7 * t3 * t3 * t12kP
         * k * k * t5 * t2 * t2 - 8 * t42kP * t13P * t5 * t1P * t7 * t2 * t2 - 8 * t42kP * t6 * t14P * t3 * t1P * t7 + 16 * t42kP * t2 * t1P
         * k * k * t6 * t1 * t3 + 12 * t42kP * t3 * t3 * t5 * t5 * t1P * k * t1 * t2 - t7 * t3P * t12kP * k * k * t5 - 2 * t42kP * t5 * t5 * t1P * k * k
         * t1 - 2 * t42kP * t5 * t1P * k * t2 * t2 - 4 * t42kP * t2 * t5 * t1P * k * k * t7 * t3 * t3 - 2 * t6 * t6 * t22kP * t3 * t3 * t4 * t1P * k * k
         + 12 * t42kP * t5 * t1P * k * t7 * t1 + t4P * t7 * t4 * t12kP * k * k * t5 * t3 * t3 - 8 * t42kP * t1P * k * t13P * t6 + 16 * t42kP
         * t13P * t5 * t1P * t2 - 16 * t42kP * t13P * t5 * t1P * t6 * t2 * t2 * t3 - 2 * t6 * t6 * t22kP * t3 * t3 * t1P * k * k * t1 * t4 * t4 - 4
         * t7 * t3P * t12kP * k * k * t1 * t5 * t2 * t4 - 2 * t42kP * t6 * t6 * t1P * k * k * t2 * t2 * t3 * t1 * t1 - t42kP * t7 * t7 * t1P * k * k - 2
         * t42kP * t6 * t6 * t1P * t1 * t1 - t4P * t12kP * k * k * t5 + t4P * t6 * t12kP * k * k * t5 * t3 * t3 * t4 - 4 * t7 * t7 * t32kP * t1P
         * k * k * t1 * t2 * t2 * t4 + 24 * t42kP * t6 * t6 * t2 * t3 * t1P * k * t1 - 2 * t42kP * t5 * t1P * k * k * t1 * t1 * t7 - 48 * t42kP * t7 * t2 * t1P
         * k * t1 * t3 + 2 * t7 * t7 * t32kP * t4 * t4 * t1P * k * t2 + 12 * t42kP * t2 * t2 * t5 * t1P * k * t1 * t7 + 2 * t4P * t2 * t2 * t5 * t5 * t12kP
         * k * k * t3 * t4 * t1 + t4P * t7 * t2 * t2 * t3 * t3 * t4 * t12kP * k * k * t5 + 8 * t42kP * t2 * t1P * k * k * t6 * t3 * t1 * t1 + t5 * t5 * t13kP
         * k * t3 * t3 - 2 * t42kP * t5 * t1P * k * k * t6 * t3 * t3 + 2 * t4P * t5 * t5 * t12kP * k * k * t1 * t3 - 2 * t42kP * t3 * t3 * t5 * t5 * t1P
         * t1 * t1 - 2 * t6 * t6 * t22kP * t3 * t3 * t1P * t1 * t1 - 24 * t42kP * t1P * k * t1 * t6 * t2 + 12 * t42kP * t6 * t1P * k * t1 * t7 + 2 * t4P
         * t2 * t2 * t5 * t5 * t12kP * k * k * t3 - 8 * t42kP * t14P * t5 * t1P * t7 * t2 - t42kP * t6 * t6 * t1P * k * k * t2 * t2 * t3 * t3 * t1 * t1 - 2
         * t42kP * t2 * t2 * t3 * t3 * t1P * t1 * t1 + 4 * t42kP * t1P * k * k * t1 * t7 * t3 * t3 + t6 * t6 * t22kP * t4 * t4 * t1P * k + 4 * t5 * t5
         * t13kP * t2 * t2 * t4 * k * t3 + 4 * t7 * t7 * t32kP * t1P * k * t13P * t4 * t4 + t4P * t7 * t2 * t2 * t12kP * k * k * t1 * t5 - 2 * t42kP
         * t1P * t1 * t1 - 18 * t42kP * t5 * t1P * k * t1 * t1 + 8 * t7 * t7 * t32kP * t1P * k * t13P * t2 * t4 * t4 + 8 * t42kP * t6 * t1P * k
         * k * t1 * t3 - 2 * t42kP * t2 * t2 * t1P * k * k * t1 - 2 * t6 * t2P * t2 * t4 * t12kP * k * k * t5 - 8 * t42kP * t6 * t13P * t3 * t3 * t1P
         * t7 * t2 * t2 + 18 * t42kP * t3 * t3 * t5 * t1P * k * t1 * t1 * t6 - 8 * t42kP * t7 * t1P * t1 * t1 * t5 * t3 - 2 * t6 * t2P * t3 * t3 * t12kP
         * k * k * t1 * t5 * t4 + 18 * t42kP * t2 * t2 * t5 * t1P * k * t1 * t1 * t7 - 2 * t7 * t7 * t32kP * t1P * t1 * t1 * t2 * t2 - 2 * t42kP * t7 * t7
         * t1P * k * k * t1 - 2 * t42kP * t6 * t6 * t14P * t3 * t3 * t1P - 2 * t42kP * t2 * t1P * k * k * t3 * t3 * t1 * t1 + 18 * t6 * t6 * t22kP
         * t3 * t3 * t1P * k * t1 * t1 * t4 - t42kP * t2 * t2 * t1P * k * k * t3 * t3 * t1 * t1 - 4 * t42kP * t6 * t14P * t3 * t3 * t1P * t7 + 12 * t42kP
         * t6 * t6 * t3 * t1P * k * t1 - 4 * t42kP * t7 * t2 * t2 * t1P * k * t3 - 2 * t6 * t2P * t2 * t4 * t4 * t12kP * k * k * t5 * t3 - 24 * t42kP * t2
         * t5 * t1P * k * t1 + 4 * t42kP * t2 * t2 * t5 * t5 * t1P * k * t13P - 4 * t42kP * t2 * t5 * t1P * k * k * t7 * t1 * t1 + 18 * t42kP * t2
         * t3 * t3 * t1P * k * t1 * t1 + 8 * t5 * t12kP * t2 * t2 * t4 * k * t4P * t1 + 4 * t5 * t12kP * t2 * t2 * t4 * k * t4P * t3 + 4 * t5 * t12kP * t2
         * t2 * t4 * k * t7 * t3P + 4 * t5 * t12kP * t2 * t4 * t4 * k * t7 * t3P + 16 * t5 * t12kP * t2 * t4 * k * t4P * t1 + 2 * t5 * t12kP * t2 * t2
         * t4 * k * t4P + 4 * t5 * t12kP * t2 * t4 * t4 * k * t7 * t3P * t3 + 4 * t5 * t12kP * t2 * t2 * t4 * k * t7 * t3P * t3 + 2 * t5 * t12kP * t2 * t2
         * t4 * k * t4P * t3 * t3 + 8 * t7 * t3P * t1P * k * t13P * t6 * t2P + 18 * t7 * t3P * t1P * k * t1 * t1 * t6 * t2P * t3 + 36 * t7 * t3P
         * t1P * k * t1 * t1 * t6 * t2P * t4 - 4 * t7 * t3P * t1P * k * k * t1 * t4P * t3 * t4 - 8 * t7 * t3P * t1P * k * k * t1 * t6 * t2P * t4 * t2 - 8
         * t4P * t14P * t1P * t7 * t3P * t2 * t4 - 4 * t4P * t14P * t1P * t7 * t3P * t4 - 8 * t4P * t14P * t1P * t7 * t3P * t2 - 16
         * t4P * t13P * t1P * t7 * t3P * t2 * t4 + 6 * t5 * t12kP * t13P * t4 * t4P * t6 * t2 * t2 * t3 - 18 * t7 * t7 * t3P * t1P * k * t1
         * t1 * t4P - 18 * t7 * t3P * t1P * k * t1 * t1 * t4P * t6 - 18 * t4P * t6 * t6 * t4 * t1P * k * t1 * t1 * t2P - 8 * t4P * t6 * t2 * t2 * t3
         * t1P * k * t13P * t7 * t3P - 2 * t6 * t2P * t3 * t3 * t4 * t1P * k * k * t4P + 4 * t6 * t6 * t2P * t14P * t1P * t4P * t4 + 12 * t5
         * t12kP * t3 * k * t1 * t1 * t4P - 4 * t6 * t2P * t3 * t4 * t1P * k * k * t7 * t3P - 4 * t7 * t3P * t2 * t3 * t1P * k * k * t4P - 2 * t5 * t5
         * t13kP * t2 * t2 * t1 * t1 * t4 + 4 * t4P * t7 * t7 * t14P * t2 * t2 * t4 * t1P * t3P * t3 - 2 * t5 * t5 * t13kP * t3 * t3 * t1 * t1 * t2 - 4
         * t5 * t5 * t13kP * t3 * t1 * t1 * t4 - 2 * t5 * t5 * t13kP * t3 * t3 * t1 * t1 * t4 - 4 * t5 * t5 * t13kP * t3 * t3 * t1 * t1 * t2 * t4 + 8 * t5 * t5 * t13kP
         * k * t2 * t3 * t4 * t4 * t1 + 2 * t7 * t3P * t2 * t2 * t3 * t4 * t1P * k * k * t4P * t6 - 18 * t4P * t6 * t4 * t1P * k * t1 * t1 * t7 * t3P + 4 * t6
         * t2P * t2 * t4 * t1P * k * k * t4P * t7 * t3 + 4 * t6 * t6 * t2P * t2 * t4 * t1P * k * k * t4P * t1 + 4 * t6 * t6 * t2P * t2 * t4 * t1P * k
         * k * t4P * t3 + 4 * t4P * t6 * t6 * t1P * t1 * t1 * t2P - 4 * t4P * t2 * t1P * k * k * t6 * t2P * t1 - 4 * t4P * t2 * t1P * k * k * t6
         * t2P * t3 + 4 * t4P * t6 * t2 * t2 * t1P * t1 * t1 * t7 * t3P * t4 * t3 + 4 * t4P * t6 * t6 * t2 * t1P * t1 * t1 * t2P * t3 * t3 * t4 - 2 * t4P
         * t2 * t3 * t3 * t4 * t1P * k * k * t6 * t2P + 2 * t6 * t2P * t2 * t3 * t3 * t1P * k * t4P + 4 * t6 * t2P * t2 * t3 * t1P * k * t7 * t3P * t4 - 36
         * t4P * t7 * t2 * t1P * k * t6 * t2P * t3 * t1 * t1 + 16 * t4P * t6 * t6 * t13P * t3 * t1P * t2P + 4 * t6 * t2P * t4 * t1P * k * t7
         * t3P * t2 + 2 * t6 * t2P * t4 * t4 * t1P * k * t7 * t3P + 4 * t4P * t2 * t5 * t1P * k * k * t6 * t2P * t1 + 4 * t4P * t2 * t5 * t1P * k
         * k * t6 * t2P * t3 + 8 * t4P * t4 * t1P * k * t6 * t2P * t13P * t2 + 12 * t4P * t4 * t1P * k * t6 * t2P * t1 * t2 + 2 * t7 * t3P * t1P
         * k * k * t4P * t6 * t4 + 2 * t7 * t7 * t3P * t1P * k * k * t4P * t4 + 2 * t7 * t3P * t1P * k * k * t4P * t5 * t3 * t4 - 2 * t7 * t3P * t1P
         * k * k * t6 * t2P * t4 * t4 + 2 * t7 * t3P * t1P * k * k * t4P * t5 * t4 + 4 * t4P * t14P * t5 * t1P * t7 * t3P + 4 * t4P * t6 * t1P
         * t14P * t7 * t3P - 12 * t7 * t7 * t3P * t4 * t1P * k * t1 * t4P - 8 * t7 * t3P * t3 * t5 * t1P * k * t13P * t4P * t2 * t2 - 4 * t7
         * t3P * t1P * k * k * t1 * t4P * t2 * t2 * t4 + 4 * t7 * t3P * t1P * k * k * t1 * t4P * t6 * t3 * t4 - 8 * t4P * t6 * t6 * t2 * t3 * t3 * t1P
         * k * t13P * t2P + 4 * t4P * t6 * t1P * t1 * t1 * t7 * t3P + 4 * t4P * t6 * t6 * t14P * t3 * t3 * t1P * t2P * t4 * t2 + 2 * t6 * t2P
         * t4 * t1P * k * k * t1 * t1 * t4P * t7 - 18 * t4P * t7 * t2 * t1P * k * t6 * t2P * t1 * t1 - 4 * t5 * t12kP * k * t4P * t6 * t2 * t3 * t3 + 4
         * t7 * t3P * t1P * k * k * t1 * t1 * t4P * t6 * t2 + 4 * t7 * t3P * t1P * k * k * t1 * t1 * t4P * t6 * t2 * t3 - 2 * t4P * t7 * t7 * t2 * t2 * t1P
         * k * t3P * t4 - 16 * t5 * t12kP * t3 * t4 * k * t1 * t4P * t6 * t2 * t2 + 16 * t7 * t3P * t1P * k * t13P * t4P * t2 + 8 * t7 * t3P * t1P
         * k * t13P * t4P * t3 + 8 * t4P * t6 * t13P * t3 * t1P * t7 * t3P * t2 * t2 + 36 * t4P * t1P * k * t1 * t1 * t6 * t2P * t3 + 18 * t4P
         * t4 * t1P * k * t6 * t2P * t1 * t1 + 8 * t4P * t4 * t1P * k * t6 * t2P * t13P + 12 * t4P * t3 * t4 * t1P * k * t7 * t3P * t1 * t2 * t2
         + 24 * t5 * t12kP * t3 * t4 * k * t1 * t1 * t6 * t2P + 32 * t5 * t12kP * t3 * t4 * k * t1 * t6 * t2P * t2 - 6 * t5 * t12kP * t13P * t4 * t6
         * t2P * t3 * t3 * t2 + 2 * t6 * t2P * t1P * k * k * t4P * t7 * t3 * t3 + 4 * t7 * t3P * t14P * t3 * t4 * t5 * t1P * t4P + 8 * t7 * t3P
         * t13P * t3 * t4 * t5 * t1P * t4P - 18 * t6 * t6 * t2P * t2 * t1P * k * t1 * t1 * t4P + 2 * t5 * t12kP * k * t4P * t3 * t3 * t4 + 8 * t4P
         * t6 * t6 * t2 * t1P * t13P * t2P * t4 - 18 * t4P * t2 * t5 * t1P * k * t1 * t1 * t6 * t2P - 12 * t4P * t2 * t5 * t1P * k * t1 * t6 * t2P - 24
         * t4P * t2 * t5 * t1P * k * t1 * t7 * t3P + 36 * t7 * t3P * t1P * k * t1 * t1 * t4P * t2 * t3 - 8 * t4P * t7 * t7 * t2 * t2 * t4 * t1P * k
         * t13P * t3P * t3 - 12 * t5 * t12kP * t3 * t4 * k * t1 * t1 * t4P * t7 - 32 * t5 * t12kP * t3 * t4 * k * t1 * t4P * t7 * t2 + 16 * t7 * t3P
         * t1P * k * t13P * t6 * t2P * t4 * t2 + 4 * t6 * t2P * t2 * t3 * t1P * k * t4P + 4 * t4P * t2 * t5 * t1P * k * k * t7 * t3P * t1 * t1
         + 4 * t4P * t2 * t5 * t1P * k * k * t6 * t2P * t3 * t4 * t1 * t1 + 8 * t6 * t2P * t3 * t1P * k * k * t1 * t4P * t7 - 32 * t5 * t5 * t12kP * t3
         * t4 * k * t1 * t4P * t2 - 8 * t5 * t5 * t12kP * t3 * t3 * t4 * k * t1 * t4P - 12 * t5 * t5 * t12kP * t3 * t4 * k * t1 * t1 * t4P + 4 * t4P * t2
         * t5 * t1P * k * k * t6 * t2P * t1 * t4 + 8 * t4P * t2 * t5 * t1P * k * k * t6 * t2P * t1 * t3 + 4 * t4P * t2 * t5 * t1P * k * k * t6 * t2P
         * t3 * t4 + 4 * t4P * t2 * t2 * t5 * t1P * k * k * t7 * t3P * t1 + 2 * t4P * t2 * t2 * t5 * t1P * k * k * t7 * t3P * t4 + 8 * t4P * t2 * t5 * t1P
         * k * k * t6 * t2P * t1 * t3 * t4 - 12 * t4P * t7 * t7 * t2 * t2 * t1P * k * t3P * t1 * t4 - 32 * t5 * t5 * t12kP * t2 * k * t1 * t4P * t3 - 6 * t5
         * t5 * t12kP * t2 * t2 * k * t1 * t1 * t4P - 24 * t5 * t5 * t12kP * t2 * k * t1 * t1 * t4P * t3 - 16 * t5 * t5 * t12kP * t2 * t2 * k * t1 * t4P
         * t3 - 12 * t5 * t12kP * t3 * t4 * k * t1 * t1 * t4P * t6 - 8 * t5 * t12kP * t3 * t3 * t4 * k * t1 * t4P * t6 - 12 * t5 * t5 * t12kP * t2 * k * t1
         * t1 * t4P - 8 * t5 * t5 * t12kP * t2 * t2 * k * t1 * t4P - 2 * t6 * t2P * t2 * t4 * t1P * k * k * t4P * t1 * t1 + 2 * t6 * t2P * t2 * t4 * t1P
         * k * k * t4P * t7 + 6 * t5 * t12kP * t13P * t4P * t6 * t2 - 8 * t6 * t2P * t13P * t1P * t7 * t3P + 4 * t5 * t12kP * k * t6 * t2P
         * t3 - 8 * t7 * t3P * t13P * t3 * t1P * t4P * t4 - 24 * t4P * t7 * t2 * t1P * k * t6 * t2P * t1 * t3 * t4 - 18 * t6 * t2P * t3 * t3 * t1P
         * k * t1 * t1 * t4P * t7 * t4 - 8 * t6 * t2P * t3 * t3 * t1P * k * t13P * t4P * t7 + 8 * t7 * t7 * t3P * t4 * t1P * t13P * t4P + 6 * t5
         * t5 * t12kP * t3 * t3 * t4 * t13P * t4P * t2 - 6 * t5 * t12kP * t13P * t4P * t2 - 6 * t5 * t12kP * t13P * t7 * t3P * t2 * t3
         + 4 * t4P * t2 * t4 * t5 * t1P * t1 * t1 * t6 * t2P * t3 * t3 + 8 * t4P * t2 * t4 * t5 * t1P * t1 * t1 * t7 * t3P * t3 + 4 * t7 * t7 * t3P * t2
         * t3 * t4 * t1P * k * k * t4P - 4 * t7 * t3P * t1P * k * k * t6 * t2P * t4 - 2 * t7 * t3P * t1P * k * k * t6 * t2P + 4 * t7 * t3P * t1P
         * k * k * t4P * t6 * t2 - 16 * t6 * t2P * t13P * t1P * t4P * t3 - 4 * t6 * t2P * t14P * t1P * t4P - 18 * t4P * t7 * t7 * t2 * t2
         * t1P * k * t3P * t1 * t1 + 4 * t7 * t7 * t3P * t14P * t3 * t1P * t4P * t2 * t2 - 3 * t5 * t12kP * t1 * t1 * t6 * t2P * t3 * t3 * t2 + 8
         * t4P * t6 * t6 * t14P * t3 * t1P * t2P * t2 + 4 * t6 * t2P * t1P * t1 * t1 * t4P * t5 + 36 * t4P * t2 * t3 * t1P * k * t1 * t1 * t6
         * t2P + 12 * t4P * t2 * t3 * t3 * t1P * k * t1 * t6 * t2P - 12 * t7 * t3P * t2 * t2 * t1P * k * t1 * t4P * t6 + 24 * t7 * t3P * t2 * t1P
         * k * t1 * t4P * t3 + 2 * t4P * t1P * k * t7 * t3P + 4 * t4P * t6 * t14P * t3 * t1P * t7 * t3P * t2 * t2 + 16 * t7 * t3P * t1P * k
         * t13P * t6 * t2P * t3 * t4 * t2 - 2 * t7 * t3P * t12kP * k * k * t5 * t4 + 2 * t4P * t7 * t2 * t12kP * k * k * t1 * t5 + 4 * t42kP * t1P
         * k * k * t1 * t1 * t7 * t3 + 4 * t42kP * t5 * t1P * k * t6 * t3 + 16 * t42kP * t6 * t2 * t1P * t13P - 2 * t42kP * t6 * t6 * t2 * t2 * t1P
         * t1 * t1 * t3 * t3 - 4 * t42kP * t13P * t5 * t5 * t1P + 36 * t42kP * t2 * t5 * t1P * k * t1 * t1 * t6 + 4 * t4P * t6 * t12kP * k * k * t5
         * t2 * t3 - 2 * t4P * t2 * t12kP * k * k * t5 * t4 * t1 - 4 * t6 * t6 * t22kP * t14P * t1P * t3 * t3 * t4 - 2 * t42kP * t2 * t1P * k * k * t3
         * t3 + 9 * t42kP * t2 * t2 * t3 * t3 * t1P * k * t1 * t1 - 16 * t42kP * t6 * t2 * t1P * t13P * t7 + t6 * t6 * t22kP * t3 * t3 * t1P * k - 4
         * t6 * t6 * t22kP * t13P * t1P * t3 * t3 * t4 * t4 - 8 * t42kP * t7 * t2 * t2 * t1P * k * t13P * t3 * t3 - 2 * t42kP * t2 * t2 * t5 * t1P
         * k * k * t7 * t1 * t1 - 2 * t42kP * t14P * t5 * t5 * t1P * t2 * t2 + 12 * t42kP * t5 * t1P * k * t6 * t1 - 2 * t6 * t6 * t22kP * t14P * t1P
         * t4 * t4 - 16 * t42kP * t13P * t5 * t1P * t6 * t2 - 8 * t42kP * t7 * t2 * t2 * t1P * k * k * t1 * t6 * t3 - 4 * t42kP * t5 * t1P * k * k
         * t1 * t7 + t5 * t5 * t13kP * k * t2 * t2 * t3 * t3 - 4 * t42kP * t7 * t2 * t1P * k - 18 * t42kP * t6 * t2 * t2 * t3 * t3 * t1P * k * t1 * t1 + t42kP
         * t5 * t5 * t1P * k * t3 * t3 - 4 * t42kP * t2 * t5 * t5 * t1P * k * k * t3 * t1 * t1 - 4 * t42kP * t14P * t5 * t1P * t6 * t2 * t2 + 2 * t42kP
         * t2 * t2 * t1P * k * k * t7 * t3 * t3 + 18 * t42kP * t6 * t6 * t2 * t2 * t3 * t1P * k * t1 * t1 - 8 * t42kP * t7 * t2 * t2 * t1P * k * t13P - 2
         * t42kP * t14P * t1P * t2 * t2 * t3 * t3 - 18 * t42kP * t7 * t2 * t2 * t1P * k * t1 * t1 + 8 * t42kP * t6 * t1P * k * t13P * t7 - 4
         * t7 * t7 * t32kP * t1P * k * k * t1 * t2 - 36 * t42kP * t1P * k * t1 * t1 * t6 * t2 + 8 * t42kP * t5 * t5 * t3 * t3 * t1P * k * t13P * t2 - 2
         * t6 * t2P * t2 * t3 * t12kP * k * k * t5 + 4 * t42kP * t7 * t2 * t1P * k * k * t1 * t1 * t3 * t3 + 6 * t42kP * t6 * t6 * t3 * t3 * t1P * k * t1
         + 4 * t42kP * t5 * t1P * k * k * t1 * t1 * t3 - t4P * t2 * t2 * t12kP * k * k * t5 * t1 - 72 * t42kP * t3 * t5 * t1P * k * t1 * t1 * t2 + 4 * t42kP
         * t2 * t3 * t3 * t5 * t1P * k * t7 - 4 * t6 * t6 * t22kP * t4 * t1P * t1 * t1 + 8 * t7 * t3P * t1P * k * t13P * t4P + 2 * t7 * t3P * t1P
         * k * k * t4P * t6 - 8 * t42kP * t6 * t1P * t13P * t7 - 2 * t42kP * t7 * t7 * t3 * t1P * k * k - 4 * t42kP * t6 * t1P * k * t2 * t3 * t3
         + 6 * t6 * t6 * t22kP * t1P * k * t1 * t4 * t4 - 4 * t6 * t6 * t22kP * t13P * t1P - 4 * t42kP * t14P * t5 * t1P * t7 * t2 * t2 + 2 * t4P
         * t6 * t12kP * k * k * t5 * t2 * t3 * t3 * t1 + 2 * t4P * t2 * t5 * t5 * t12kP * k * k * t1 - 2 * t7 * t7 * t32kP * t1P * k * k * t4 - 2 * t7 * t7 * t32kP
         * t1P * k * k * t1 + 8 * t42kP * t2 * t2 * t5 * t1P * k * k * t1 * t3 + 4 * t42kP * t7 * t2 * t1P * k * t6 + 6 * t42kP * t6 * t6 * t2 * t2 * t3
         * t3 * t1P * k * t1 - 2 * t7 * t7 * t32kP * t14P * t2 * t2 * t4 * t4 * t1P + 2 * t42kP * t6 * t1P * k * k * t1 * t1 + 2 * t4P * t6 * t12kP
         * k * k * t5 * t2 * t2 * t3 - t42kP * t1P * k * k * t1 * t1 + 2 * t5 * t5 * t13kP * k * t3 * t3 * t4 - t42kP * t5 * t5 * t1P * k * k * t1 * t1 + 6 * t42kP
         * t3 * t3 * t5 * t5 * t1P * k * t1 + 8 * t42kP * t6 * t2 * t1P * t1 * t1 - 4 * t42kP * t13P * t1P * t3 * t3 - t4P * t4 * t12kP * k * k
         * t5 * t3 * t3 + t4P * t5 * t5 * t12kP * k * k * t1 * t3 * t3 - 2 * t42kP * t5 * t5 * t1P * k * k * t1 * t3 * t3 + 4 * t42kP * t6 * t14P * t3
         * t3 * t1P + 9 * t42kP * t6 * t6 * t2 * t2 * t3 * t3 * t1P * k * t1 * t1 - t6 * t6 * t22kP * t1P * k * k * t4 * t4 + 24 * t42kP * t7 * t7 * t2
         * t1P * k * t1 * t3 + 9 * t42kP * t5 * t5 * t3 * t3 * t1P * k * t1 * t1 + 2 * t7 * t7 * t32kP * t4 * t1P * k * t2 * t2 + 16 * t42kP * t7 * t3
         * t3 * t5 * t1P * k * t13P * t2 - 72 * t42kP * t7 * t2 * t1P * k * t3 * t1 * t1 + t7 * t7 * t32kP * t4 * t4 * t1P * k - 36 * t42kP * t1P
         * k * t1 * t1 * t7 * t3 + 8 * t6 * t6 * t22kP * t3 * t3 * t1P * k * t13P * t4 + 4 * t42kP * t6 * t1P * k * k * t3 + 18 * t42kP * t7 * t2 * t2
         * t1P * k * t6 * t3 * t3 * t1 * t1 - 8 * t42kP * t1P * k * t13P * t7 + 12 * t7 * t7 * t32kP * t2 * t2 * t1P * k * t1 * t4 + 4 * t6 * t6 * t22kP
         * t4 * t1P * k * t3 - 4 * t7 * t3P * t2 * t3 * t4 * t12kP * k * k * t5 + 4 * t7 * t7 * t32kP * t4 * t1P * k * t2 - 8 * t6 * t6 * t22kP * t13P
         * t1P * t3 - 4 * t42kP * t2 * t5 * t5 * t1P * t1 * t1 - 4 * t42kP * t7 * t7 * t2 * t1P * k * k * t1 * t1 * t3 - t4P * t12kP * k * k * t1 * t5
         * t4 + 8 * t7 * t7 * t32kP * t1P * k * t13P * t2 - 2 * t4P * t12kP * k * k * t1 * t5 * t3 + 2 * t42kP * t6 * t1P * k * t7 * t3 * t3 - 16 * t42kP
         * t6 * t3 * t5 * t1P * k * k * t1 * t2 - t6 * t2P * t2 * t4 * t4 * t12kP * k * k * t5 * t3 * t3 + 2 * t4P * t6 * t12kP * k * k * t5 * t2 * t2 * t3 * t4
         + 36 * t42kP * t6 * t6 * t2 * t3 * t1P * k * t1 * t1 + 4 * t42kP * t7 * t7 * t2 * t1P * k * t3 - 4 * t42kP * t7 * t2 * t2 * t1P * k * k * t1 * t1
         * t6 * t3 - 8 * t42kP * t14P * t5 * t1P * t7 * t3 + 36 * t42kP * t7 * t3 * t5 * t1P * k * t1 * t1 * t2 * t2 + 4 * t42kP * t14P * t5 * t1P
         * t2 * t2 - 8 * t42kP * t2 * t5 * t1P * k * k * t7 * t3 - 24 * t42kP * t3 * t3 * t5 * t1P * k * t1 * t2 - 4 * t6 * t2P * t3 * t4 * t12kP * k * k
         * t5 - 2 * t7 * t3P * t12kP * k * k * t1 * t5 * t2 * t2 * t4 + 4 * t4P * t6 * t12kP * k * k * t5 * t2 * t3 * t4 + 2 * t42kP * t7 * t2 * t2 * t1P
         * k * k * t1 * t1 - 2 * t4P * t2 * t12kP * k * k * t5 * t1 + t42kP * t2 * t2 * t3 * t3 * t5 * t5 * t1P * k - 2 * t7 * t7 * t32kP * t2 * t2 * t4 * t1P
         * k * k + 4 * t5 * t5 * t13kP * t2 * t4 * k - 2 * t42kP * t6 * t6 * t1P * k * k * t3 * t1 * t1 - 4 * t42kP * t6 * t6 * t1P * k * k * t1 * t2 * t2 * t3
         + 4 * t42kP * t2 * t1P * k * k * t6 + 4 * t42kP * t7 * t2 * t2 * t1P * k * k * t1 + 18 * t42kP * t5 * t1P * k * t1 * t1 * t7 - 8 * t42kP * t6
         * t2 * t2 * t1P * t1 * t1 * t7 * t3 + 2 * t4P * t7 * t3 * t3 * t12kP * k * k * t5 * t2 + 16 * t42kP * t7 * t2 * t2 * t3 * t1P * t13P - 12 * t42kP
         * t1P * k * t1 * t7 * t3 * t3 + 2 * t5 * t5 * t13kP * k * t2 * t2 * t3 + 2 * t42kP * t2 * t2 * t5 * t1P * k * k - 4 * t6 * t6 * t22kP * t4 * t1P
         * t1 * t1 * t3 * t3 - 2 * t6 * t6 * t22kP * t1P * k * k * t4 + t5 * t5 * t13kP * k - 4 * t42kP * t13P * t1P + t4P * t2 * t2 * t5 * t5 * t12kP
         * k * k * t3 * t3 * t1 - 16 * t42kP * t3 * t5 * t1P * k * t13P - 8 * t6 * t6 * t22kP * t3 * t1P * k * k * t1 * t4 + 2 * t42kP * t6 * t6 * t1P
         * k * t2 - 4 * t7 * t7 * t32kP * t4 * t4 * t1P * t13P - 2 * t42kP * t5 * t5 * t1P * k * k * t1 * t1 * t3 + 2 * t7 * t3P * t1P * k * k * t4P
         * t5 - 2 * t7 * t3P * t1P * k * k * t6 * t2P * t3 - 16 * t6 * t6 * t22kP * t13P * t1P * t3 * t4 - 4 * t7 * t3P * t12kP * k * k * t1 * t5
         * t2 * t3 * t4 + 8 * t42kP * t14P * t5 * t1P * t2 * t3 * t3 + 4 * t42kP * t7 * t7 * t2 * t2 * t1P * k * t13P - 8 * t42kP * t3 * t3 * t5
         * t1P * k * t13P * t2 * t2 - 2 * t7 * t7 * t32kP * t1P * t14P * t2 * t2 + t5 * t5 * t13kP * k * t3 * t3 * t4 * t4 - t6 * t2P * t3 * t3 * t12kP
         * k * k * t1 * t5 * t2 * t4 * t4 + 2 * t4P * t6 * t12kP * k * k * t5 * t2 * t3 * t3 * t4 * t1 + 24 * t42kP * t2 * t5 * t1P * k * t1 * t7 + 4 * t4P * t14P
         * t5 * t1P * t6 * t2P * t3 * t3 * t2 + 16 * t42kP * t13P * t1P * t7 * t3 - t7 * t3P * t12kP * k * k * t1 * t5 * t2 * t2 * t3 * t4 * t4 - 32
         * t42kP * t3 * t5 * t1P * k * t13P * t2 - 2 * t6 * t2P * t4 * t12kP * k * k * t1 * t5 - t7 * t7 * t32kP * t1P * k * k * t1 * t1 * t2 * t2 - 2
         * t42kP * t7 * t2 * t2 * t3 * t3 * t5 * t1P * k * k * t1 * t1 + 2 * t4P * t7 * t2 * t12kP * k * k * t1 * t5 * t3 * t3 + 8 * t42kP * t5 * t1P * k
         * t13P * t6 + 2 * t42kP * t6 * t1P * k * k * t3 * t3 + 12 * t42kP * t1P * k * t1 * t3 + 16 * t42kP * t13P * t1P * t7 * t2 + 4 * t42kP
         * t2 * t2 * t1P * k * k * t6 * t3 + 4 * t42kP * t7 * t7 * t1P * k * t13P * t3 * t3 - 8 * t42kP * t6 * t2 * t1P * t1 * t1 * t7 * t3 * t3 - 2 * t6
         * t2P * t12kP * k * k * t5 * t4 - 2 * t42kP * t7 * t1P * k - 4 * t42kP * t14P * t1P * t3 - 8 * t6 * t6 * t22kP * t4 * t1P * t1 * t1
         * t3 - 2 * t42kP * t5 * t1P * k * k * t7 - 8 * t42kP * t6 * t3 * t1P * t1 * t1 * t7 - 4 * t42kP * t6 * t6 * t2 * t2 * t1P * t1 * t1 * t3 + 2 * t42kP
         * t5 * t1P * k * k * t3 * t3 - 4 * t42kP * t13P * t1P * t2 * t2 * t3 * t3 - 4 * t42kP * t6 * t1P * k * t3 - 2 * t6 * t2P * t2 * t4 * t12kP
         * k * k * t5 * t3 * t3 - 2 * t6 * t6 * t22kP * t4 * t4 * t1P * t1 * t1 - t4P * t3 * t3 * t12kP * k * k * t5 - t42kP * t2 * t2 * t5 * t5 * t1P
         * k * k + 2 * t4P * t6 * t12kP * k * k * t5 * t2 * t4 * t1 + 12 * t6 * t6 * t22kP * t1P * k * t1 * t4 + 8 * t42kP * t2 * t5 * t5 * t1P * k * t13P
         + 4 * t42kP * t6 * t2 * t2 * t1P * t1 * t1 + 4 * t42kP * t2 * t3 * t5 * t5 * t1P * k - 4 * t42kP * t13P * t5 * t5 * t1P * t2 * t2 * t3 * t3
         + 18 * t42kP * t7 * t7 * t1P * k * t1 * t1 * t3 + 4 * t42kP * t2 * t2 * t1P * k * k * t6 * t3 * t1 * t1 + 2 * t42kP * t2 * t2 * t3 * t5 * t5 * t1P
         * k + 8 * t42kP * t6 * t14P * t3 * t3 * t1P * t2 + 24 * t42kP * t2 * t3 * t1P * k * t1 + 6 * t42kP * t7 * t7 * t1P * k * t1 + 16 * t42kP
         * t7 * t2 * t1P * k * k * t1 * t3 + 4 * t42kP * t2 * t2 * t1P * t1 * t1 * t5 + 12 * t7 * t7 * t32kP * t4 * t1P * k * t1 - 8 * t42kP * t6 * t6
         * t2 * t1P * t1 * t1 * t3 - 2 * t42kP * t6 * t6 * t1P * k * k * t2 + 9 * t42kP * t5 * t5 * t1P * k * t1 * t1 + 4 * t5 * t5 * t13kP * k * t3 * t4 - 4
         * t42kP * t6 * t3 * t3 * t5 * t1P * k * k * t1 * t1 * t2 + 24 * t42kP * t6 * t3 * t1P * k * t1 * t7 - 4 * t42kP * t6 * t1P * t14P * t7 - 8
         * t42kP * t6 * t6 * t13P * t3 * t3 * t1P * t2 - 8 * t42kP * t2 * t5 * t1P * k * k * t7 * t1 + 9 * t42kP * t1P * k * t1 * t1 * t2 * t2 - t42kP
         * t7 * t7 * t3 * t3 * t1P * k * k * t2 * t2 + t4P * t6 * t12kP * k * k * t5 * t3 * t3 * t4 * t1 - 2 * t6 * t2P * t2 * t4 * t12kP * k * k * t5 * t1 - t42kP
         * t6 * t6 * t1P * k * k - 2 * t7 * t7 * t32kP * t1P * t1 * t1 + 4 * t42kP * t6 * t1P * k * k * t3 * t1 * t1 - 4 * t42kP * t7 * t2 * t2 * t3 * t5
         * t1P * k * k * t1 * t1 - 18 * t42kP * t7 * t2 * t2 * t1P * k * t3 * t3 * t1 * t1 - 18 * t42kP * t3 * t3 * t5 * t1P * k * t1 * t1 * t2 * t2 - 4 * t42kP
         * t7 * t2 * t1P * k * k * t1 * t1 * t6 - 8 * t42kP * t7 * t2 * t3 * t3 * t5 * t1P * k * k * t1 + 6 * t6 * t6 * t22kP * t3 * t3 * t1P * k * t1 + 9 * t7
         * t7 * t32kP * t1P * k * t1 * t1 * t4 * t4 - 4 * t42kP * t6 * t3 * t5 * t1P * k * k * t1 * t1 * t2 * t2 - 2 * t42kP * t5 * t1P * k * t3 * t3 + t4P
         * t6 * t12kP * k * k * t5 * t3 * t3 * t1 - 8 * t42kP * t3 * t3 * t5 * t1P * k * t13P + t4P * t7 * t2 * t2 * t12kP * k * k * t1 * t5 * t4 - 18
         * t42kP * t3 * t3 * t5 * t1P * k * t1 * t1 + 8 * t42kP * t2 * t3 * t3 * t1P * t1 * t1 * t5 + 8 * t42kP * t14P * t1P * t7 * t3 + 8 * t6 * t6
         * t22kP * t3 * t1P * k * t13P * t4 * t4 - 4 * t42kP * t6 * t6 * t1P * t13P - 4 * t42kP * t1P * t1 * t1 * t3 - 2 * t42kP * t1P
         * k * k * t1 + t6 * t6 * t22kP * t1P * k + 4 * t42kP * t6 * t2 * t2 * t1P * t14P - 4 * t4P * t2 * t3 * t4 * t12kP * k * k * t5 + 36 * t42kP
         * t3 * t5 * t1P * k * t1 * t1 * t6 - 2 * t42kP * t6 * t1P * k * k * t7 * t2 * t2 - t4P * t12kP * k * k * t1 * t5 * t3 * t3 * t4 - 4 * t42kP * t6
         * t2 * t2 * t1P * t1 * t1 * t7 * t3 * t3 + 4 * t42kP * t2 * t2 * t1P * k * k * t6 * t1 * t3 * t3 - 8 * t7 * t7 * t32kP * t4 * t1P * t13P + 18
         * t7 * t7 * t32kP * t1P * k * t1 * t1 * t2 * t4 * t4 + 9 * t6 * t6 * t22kP * t3 * t3 * t1P * k * t1 * t1 * t4 * t4 - 4 * t42kP * t7 * t7 * t2 * t2
         * t1P * k * k * t1 * t3 - 8 * t42kP * t5 * t1P * k * t13P + t42kP * t7 * t7 * t2 * t2 * t1P * k - 4 * t42kP * t7 * t7 * t1P * t1 * t1
         * t2 + 2 * t4P * t2 * t5 * t5 * t12kP * k * k * t3 * t3 * t4 - 2 * t7 * t3P * t12kP * k * k * t1 * t5 * t3 * t4 - 24 * t42kP * t6 * t2 * t2 * t3 * t1P
         * k * t1 - t42kP * t6 * t6 * t1P * k * k * t3 * t3 - 4 * t4P * t2 * t12kP * k * k * t5 * t3 * t1 + 4 * t42kP * t2 * t2 * t5 * t1P * k * k * t1 * t3
         * t3 - 8 * t42kP * t6 * t2 * t1P * t1 * t1 * t5 * t3 * t3 - 4 * t42kP * t7 * t7 * t2 * t2 * t3 * t1P * t14P + 2 * t5 * t5 * t13kP * t2 * t2
         * t4 * k * t3 * t3 - 24 * t42kP * t7 * t2 * t2 * t1P * k * t1 * t3 - 16 * t42kP * t6 * t13P * t3 * t3 * t1P * t7 * t2 + 2 * t42kP * t6 * t6
         * t1P * k * t3 + 8 * t42kP * t13P * t5 * t1P - 2 * t42kP * t5 * t1P * k + t4P * t5 * t5 * t12kP * k * k - 8 * t7 * t7 * t32kP * t13P
         * t2 * t2 * t4 * t1P - 2 * t42kP * t6 * t6 * t1P * k * k * t2 * t2 * t3 + 2 * t4P * t7 * t2 * t3 * t3 * t4 * t12kP * k * k * t5 - 2 * t7 * t3P * t2
         * t4 * t4 * t12kP * k * k * t5 + 4 * t42kP * t7 * t2 * t2 * t1P * k * k * t1 * t1 * t3 - 16 * t42kP * t14P * t5 * t1P * t6 * t2 * t3 - 4 * t42kP
         * t2 * t5 * t5 * t1P * t1 * t1 * t3 * t3 - 4 * t42kP * t7 * t7 * t2 * t2 * t3 * t1P * t1 * t1 + 12 * t42kP * t7 * t7 * t1P * k * t1 * t3 + 12 * t42kP
         * t3 * t5 * t5 * t1P * k * t1 * t2 * t2 + t4P * t7 * t4 * t12kP * k * k * t5 * t3 * t3 * t1 - 4 * t42kP * t5 * t1P * k * k * t1 * t6 * t3 * t3 + 9 * t7
         * t7 * t32kP * t1P * k * t1 * t1 * t2 * t2 + 2 * t4P * t6 * t12kP * k * k * t5 * t3 * t1 - 2 * t42kP * t7 * t7 * t2 * t2 * t3 * t3 * t1P * t1
         * t1 - 4 * t42kP * t6 * t6 * t3 * t1P * t1 * t1 + 24 * t7 * t7 * t32kP * t2 * t1P * k * t1 * t4 + 12 * t7 * t7 * t32kP * t2 * t1P * k * t1 + 4
         * t42kP * t14P * t1P * t7 * t2 * t2 - 8 * t42kP * t7 * t7 * t2 * t3 * t1P * t14P - 2 * t4P * t4 * t12kP * k * k * t5 * t3 - 2 * t42kP
         * t7 * t7 * t3 * t1P * k * k * t2 * t2 - 2 * t7 * t7 * t32kP * t1P * k * k * t1 * t1 * t2 + 16 * t42kP * t7 * t2 * t1P * k * t6 * t13P * t3 * t3 - 8
         * t42kP * t7 * t2 * t1P * k * k * t1 * t6 + 36 * t42kP * t3 * t3 * t5 * t1P * k * t1 * t1 * t6 * t2 + 16 * t42kP * t2 * t5 * t1P * k * k * t1
         * t3 + 4 * t6 * t6 * t22kP * t1P * k * t13P - 8 * t42kP * t13P * t5 * t1P * t6 * t3 * t3 + 16 * t42kP * t5 * t5 * t3 * t1P * k * t13P
         * t2 - 4 * t6 * t2P * t3 * t12kP * k * k * t1 * t5 * t2 * t4 - t7 * t7 * t32kP * t1P * k * k + 2 * t4P * t2 * t5 * t5 * t12kP * k * k - 2 * t7 * t7
         * t32kP * t1P * k * k * t1 * t4 * t4 + 12 * t42kP * t2 * t2 * t3 * t1P * k * t1 - 2 * t42kP * t7 * t7 * t1P * t1 * t1 * t2 * t2 + t4P * t7
         * t2 * t2 * t12kP * k * k * t1 * t5 * t3 * t3 - t6 * t6 * t22kP * t4 * t4 * t1P * k * k * t1 * t1 + 18 * t42kP * t1P * k * t1 * t1 * t3 - 8 * t42kP
         * t14P * t5 * t1P * t6 * t2 + t4P * t7 * t2 * t2 * t12kP * k * k * t5 - 4 * t42kP * t7 * t7 * t1P * t13P * t3 * t3 - 4 * t42kP * t5
         * t5 * t1P * k * k * t1 * t3 + 12 * t42kP * t2 * t3 * t3 * t1P * k * t1 - 2 * t42kP * t2 * t2 * t5 * t1P * k * k * t6 * t1 * t1 + 4 * t42kP * t6
         * t6 * t3 * t3 * t1P * k * t13P + 2 * t5 * t5 * t13kP * k * t3 + 4 * t42kP * t6 * t6 * t2 * t2 * t3 * t3 * t1P * k * t13P + 18 * t7 * t7 * t32kP
         * t1P * k * t1 * t1 * t2 * t2 * t4 + 8 * t42kP * t6 * t3 * t1P * t1 * t1 + 4 * t4P * t2 * t5 * t5 * t12kP * k * k * t3 + 18 * t42kP * t5 * t5
         * t3 * t1P * k * t1 * t1 * t2 * t2 - 4 * t42kP * t2 * t1P * k * k * t1 * t3 * t3 - 8 * t42kP * t2 * t5 * t5 * t1P * k * k * t1 * t3 + 2 * t5 * t5 * t13kP
         * t2 * t4 * t4 * k + 2 * t42kP * t7 * t2 * t2 * t1P * k * t6 * t3 * t3 - 4 * t42kP * t6 * t2 * t2 * t1P * t1 * t1 * t5 * t3 * t3 - 12 * t42kP * t1P
         * k * t1 * t6 * t2 * t2 - 4 * t42kP * t14P * t5 * t1P * t7 - 2 * t42kP * t6 * t6 * t2 * t2 * t1P * t14P + 4 * t42kP * t5 * t1P * k * t7
         * t3 + 4 * t42kP * t7 * t7 * t1P * k * t13P - 16 * t42kP * t7 * t2 * t1P * k * t13P * t3 * t3 + 2 * t42kP * t3 * t1P * k - t42kP
         * t2 * t2 * t1P * k * k + t42kP * t5 * t5 * t1P * k - t42kP * t3 * t3 * t1P * k * k - 2 * t42kP * t14P * t1P * t3 * t3 + 2 * t42kP
         * t7 * t2 * t2 * t1P * k * t6 - 8 * t42kP * t5 * t1P * k * k * t1 * t6 * t3 - 4 * t7 * t7 * t32kP * t1P * k * k * t1 * t2 * t4 * t4 - 2 * t42kP
         * t2 * t2 * t1P * k * k * t3 - t6 * t2P * t12kP * k * k * t5 * t1 + 2 * t6 * t6 * t22kP * t4 * t4 * t1P * k * t3 - 4 * t6 * t2P * t2 * t4 * t12kP
         * k * k * t5 * t3 - 8 * t42kP * t13P * t1P * t2 * t2 * t3 - 2 * t42kP * t2 * t2 * t5 * t5 * t1P * k * k * t3 - 8 * t42kP * t6 * t3 * t5 * t1P
         * k * k * t1 * t1 * t2 + t4P * t7 * t4 * t12kP * k * k * t5 * t1 + 36 * t42kP * t7 * t7 * t2 * t1P * k * t3 * t1 * t1 + 9 * t42kP * t7 * t7 * t1P
         * k * t1 * t1 * t3 * t3 - 8 * t6 * t6 * t22kP * t13P * t1P * t3 * t3 * t4 - 4 * t42kP * t6 * t3 * t3 * t5 * t1P * k * k * t1 * t2 * t2 + 72 * t42kP
         * t3 * t5 * t1P * k * t1 * t1 * t6 * t2 + 8 * t42kP * t7 * t3 * t3 * t5 * t1P * k * t13P * t2 * t2 + 72 * t42kP * t7 * t2 * t1P * k * t6 * t3
         * t1 * t1 - 8 * t42kP * t7 * t13P * t2 * t2 * t3 * t3 * t5 * t1P - 4 * t42kP * t2 * t2 * t1P * k * k * t1 * t3 - 4 * t42kP * t6 * t1P * t1
         * t1 * t5 - 2 * t42kP * t2 * t5 * t5 * t1P * k * k * t1 * t1 + 4 * t42kP * t5 * t1P * k * t6 * t2 + 4 * t42kP * t7 * t1P * t1 * t1 * t3 * t3 + t4P
         * t7 * t12kP * k * k * t5 * t1 + 4 * t42kP * t1P * k * t13P * t2 * t2 - 4 * t42kP * t7 * t7 * t1P * t13P * t2 * t2 - 2 * t6 * t6 * t22kP
         * t4 * t1P * k * k * t1 * t1 - t42kP * t6 * t6 * t1P * k * k * t3 * t3 * t1 * t1 - 2 * t42kP * t5 * t1P * k * k * t6 + 12 * t42kP * t3 * t5 * t5
         * t1P * k * t1 - 16 * t42kP * t7 * t2 * t3 * t1P * t1 * t1 * t5 - t7 * t3P * t12kP * k * k * t1 * t5 + 8 * t42kP * t5 * t1P * k * k * t1 * t3 - 4
         * t7 * t7 * t32kP * t14P * t2 * t2 * t4 * t1P - 2 * t7 * t7 * t32kP * t1P * k * k * t1 * t2 * t2 * t4 * t4 - 2 * t42kP * t6 * t1P * k * k
         * t7 + 4 * t6 * t6 * t22kP * t3 * t3 * t1P * k * t13P * t4 * t4 - t42kP * t6 * t6 * t1P * k * k * t2 * t2 + 18 * t42kP * t7 * t2 * t2 * t1P
         * k * t6 * t1 * t1 + 32 * t42kP * t3 * t5 * t1P * k * t13P * t6 * t2 - 72 * t42kP * t6 * t2 * t3 * t1P * k * t1 * t1 + 8 * t42kP * t2 * t1P
         * k * k * t7 * t3 + 6 * t42kP * t1P * k * t1 - 8 * t7 * t7 * t32kP * t13P * t2 * t4 * t4 * t1P + 8 * t42kP * t2 * t5 * t1P * k * k * t1 * t3
         * t3 - 12 * t42kP * t1P * k * t1 * t6 * t3 * t3 - 2 * t42kP * t14P * t1P - 8 * t42kP * t7 * t7 * t2 * t3 * t3 * t1P * t13P - 8 * t42kP
         * t14P * t5 * t1P * t6 * t3 - 18 * t42kP * t1P * k * t1 * t1 * t6 * t2 * t2 - 12 * t42kP * t3 * t3 * t5 * t1P * k * t1 + 6 * t7 * t7 * t32kP
         * t4 * t4 * t1P * k * t1 + t4P * t2 * t2 * t5 * t5 * t12kP * k * k * t3 * t3 + 2 * t5 * t5 * t13kP * t2 * t2 * t4 * t4 * k * t3 + 8 * t42kP * t2 * t5
         * t1P * k * k * t3 - 32 * t42kP * t6 * t2 * t3 * t1P * k * t13P - 2 * t42kP * t6 * t6 * t1P * k * k * t2 * t3 * t3 * t1 * t1 - 2 * t42kP * t6
         * t6 * t3 * t3 * t1P * t1 * t1 - 4 * t42kP * t6 * t1P * t1 * t1 * t7 + 6 * t5 * t12kP * t13P * t4 * t4P * t7 * t2 * t2 * t3 - t42kP * t2
         * t2 * t1P * k * k * t3 * t3 - 36 * t42kP * t2 * t5 * t1P * k * t1 * t1 + 18 * t42kP * t2 * t2 * t5 * t1P * k * t1 * t1 * t6 + 4 * t42kP * t7
         * t7 * t2 * t2 * t1P * k * t13P * t3 * t3 + 4 * t42kP * t2 * t1P * k * k * t7 - 4 * t42kP * t7 * t7 * t3 * t1P * k * k * t2 - 2 * t42kP * t6
         * t6 * t1P * k * k * t1 * t2 * t2 * t3 * t3 + 2 * t42kP * t6 * t1P * k * t7 - 2 * t42kP * t6 * t6 * t1P * k * k * t1 + 16 * t42kP * t6 * t6 * t2
         * t3 * t1P * k * t13P + 8 * t42kP * t2 * t2 * t5 * t1P * k * t13P * t6 - 18 * t42kP * t1P * k * t1 * t1 * t7 * t3 * t3 + 48 * t42kP
         * t7 * t3 * t5 * t1P * k * t1 * t2 - t7 * t3P * t12kP * k * k * t1 * t5 * t3 - t7 * t7 * t32kP * t1P * k * k * t1 * t1 - 2 * t6 * t6 * t22kP * t3
         * t3 * t1P * k * k * t1 + t42kP * t6 * t6 * t1P * k * t2 * t2 + 4 * t42kP * t6 * t3 * t3 * t1P * t1 * t1 - 4 * t42kP * t3 * t1P * k * t7 - 4
         * t42kP * t6 * t1P * k * k * t7 * t2 * t2 * t3 + 12 * t42kP * t2 * t5 * t5 * t1P * k * t1 - 4 * t42kP * t6 * t6 * t2 * t1P * t1 * t1 - 4 * t42kP
         * t2 * t3 * t3 * t1P * t1 * t1 - t7 * t3P * t12kP * k * k * t1 * t5 * t2 * t2 * t3 + 16 * t42kP * t2 * t3 * t1P * k * t13P - 4 * t42kP * t2
         * t5 * t1P * k * k * t7 - t4P * t2 * t2 * t3 * t3 * t4 * t12kP * k * k * t5 * t1 - 8 * t42kP * t7 * t7 * t2 * t2 * t3 * t1P * t13P - 4 * t42kP
         * t6 * t6 * t2 * t1P * t14P + 18 * t42kP * t5 * t5 * t3 * t3 * t1P * k * t1 * t1 * t2 - 8 * t42kP * t6 * t6 * t2 * t1P * t13P - 4 * t42kP
         * t5 * t1P * k * k * t1 * t1 * t7 * t3 + t4P * t5 * t5 * t12kP * k * k * t3 * t3 + 18 * t42kP * t6 * t6 * t3 * t1P * k * t1 * t1 - 4 * t42kP * t2
         * t2 * t3 * t5 * t1P * k + 24 * t42kP * t7 * t2 * t1P * k * t6 * t1 * t3 * t3 - 4 * t6 * t6 * t22kP * t3 * t1P * k * k * t1 * t4 * t4 - 2 * t7 * t7
         * t32kP * t1P * k * k * t1 * t1 * t4 + 24 * t42kP * t7 * t2 * t2 * t1P * k * t6 * t1 * t3 + 16 * t42kP * t7 * t2 * t3 * t3 * t1P * t13P
         + 6 * t42kP * t3 * t3 * t5 * t5 * t1P * k * t1 * t2 * t2 - 4 * t4P * t2 * t12kP * k * k * t5 * t3 + 4 * t42kP * t7 * t2 * t2 * t1P * k * t6 * t3 - t6
         * t2P * t3 * t3 * t4 * t4 * t12kP * k * k * t5 + 16 * t42kP * t6 * t13P * t3 * t1P - 4 * t6 * t6 * t22kP * t13P * t1P * t3 * t3 - 2
         * t42kP * t1P * t1 * t1 * t3 * t3 - t6 * t6 * t22kP * t1P * k * k * t3 * t3 - 4 * t42kP * t14P * t5 * t5 * t1P * t2 * t2 * t3 - 2 * t42kP
         * t7 * t2 * t2 * t1P * k * k * t1 * t1 * t6 + 2 * t42kP * t6 * t1P * k * k * t3 * t3 * t1 * t1 + t7 * t7 * t32kP * t2 * t2 * t1P * k - 4 * t42kP
         * t14P * t5 * t1P * t6 * t3 * t3 - 2 * t42kP * t14P * t5 * t5 * t1P * t3 * t3 + 9 * t6 * t6 * t22kP * t1P * k * t1 * t1 + 2 * t42kP
         * t7 * t3 * t3 * t1P * k * k - 4 * t42kP * t7 * t7 * t2 * t2 * t3 * t3 * t1P * t13P + t42kP * t2 * t2 * t3 * t3 * t1P * k - 8 * t7 * t7 * t32kP
         * t1P * k * k * t1 * t2 * t4 - 8 * t42kP * t6 * t2 * t2 * t3 * t3 * t1P * k * t13P + t5 * t5 * t13kP * t2 * t2 * t4 * t4 * k * t3 * t3 + 4 * t42kP
         * t5 * t5 * t3 * t3 * t1P * k * t13P * t2 * t2 + 6 * t42kP * t6 * t6 * t1P * k * t1 + 2 * t4P * t6 * t12kP * k * k * t5 * t2 * t3 * t3 * t4 + 2
         * t4P * t2 * t5 * t5 * t12kP * k * k * t3 * t3 * t4 * t1 + 18 * t42kP * t5 * t5 * t3 * t1P * k * t1 * t1 + 2 * t4P * t5 * t5 * t12kP * k * k * t3 - 8
         * t42kP * t14P * t5 * t5 * t1P * t2 * t3 + 8 * t42kP * t1P * k * k * t1 * t7 * t3 + 36 * t42kP * t7 * t3 * t5 * t1P * k * t1 * t1 + 6 * t6
         * t6 * t22kP * t1P * k * t1 + 12 * t42kP * t3 * t3 * t5 * t1P * k * t1 * t6 - 2 * t42kP * t2 * t2 * t1P * k * k * t1 * t3 * t3 + 4 * t7 * t7 * t32kP
         * t1P * k * t13P * t2 * t2 * t4 * t4 - 2 * t6 * t2P * t3 * t3 * t12kP * k * k * t1 * t5 * t2 * t4 + 2 * t4P * t7 * t3 * t12kP * k * k * t5 * t2
         * t2 - 8 * t42kP * t6 * t2 * t2 * t1P * t13P * t7 + 16 * t42kP * t6 * t2 * t1P * t1 * t1 * t3 - 4 * t42kP * t7 * t2 * t3 * t3 * t5 * t1P
         * k * k * t1 * t1 + 2 * t4P * t7 * t3 * t12kP * k * k * t5 * t1 + 8 * t42kP * t6 * t2 * t1P * t14P - t42kP * t7 * t7 * t1P * k * k * t1 * t1
         + 4 * t4P * t2 * t5 * t5 * t12kP * k * k * t3 * t4 * t1 + 9 * t42kP * t1P * k * t1 * t1 * t3 * t3 - 2 * t4P * t2 * t2 * t12kP * k * k * t5 * t3 - 12
         * t42kP * t2 * t2 * t5 * t1P * k * t1 + 2 * t4P * t2 * t2 * t5 * t5 * t12kP * k * k * t3 * t4 - 4 * t42kP * t2 * t1P * t1 * t1 - 4 * t42kP
         * t7 * t7 * t1P * t13P + 12 * t42kP * t3 * t3 * t5 * t1P * k * t1 * t6 * t2 * t2 - 2 * t7 * t7 * t32kP * t2 * t4 * t4 * t1P * k * k + 4 * t42kP
         * t1P * t1 * t1 * t5 * t3 * t3 - 2 * t42kP * t6 * t6 * t1P * k * k * t1 * t3 * t3 - 2 * t42kP * t6 * t3 * t3 * t5 * t1P * k * k * t1 * t1 * t2 * t2
         + 8 * t42kP * t5 * t5 * t3 * t1P * k * t13P * t2 * t2 - t6 * t2P * t12kP * k * k * t5 - 16 * t42kP * t7 * t2 * t2 * t1P * k * t13P * t3
         + 16 * t42kP * t3 * t5 * t1P * k * t13P * t6 * t2 * t2 - 4 * t42kP * t13P * t5 * t5 * t1P * t3 * t3 - 4 * t42kP * t6 * t1P * k * k * t7
         * t1 * t3 * t3 - 4 * t6 * t6 * t22kP * t3 * t1P * k * k * t1 - 2 * t7 * t3P * t12kP * k * k * t5 * t3 * t4 - 4 * t42kP * t14P * t5 * t1P * t6
         * t2 * t2 * t3 * t3 + 18 * t42kP * t7 * t7 * t2 * t1P * k * t3 * t3 * t1 * t1) * t1aP * t1bP * Math.Pow((t7 * t3P + t6 * t2P
         + t5 * t1P + t4P - t4P * t5 * t1 * t2 * t3 + t5 * t1P * t2 + t6 * t2P * t4 + t6 * t2P * t3 + t6 * t2P * t1 + t7 * t3P * t4 + t7 * t3P
         * t2 + t7 * t3P * t1 - t4P * t6 * t2 - t4P * t7 * t3 - t4P * t5 * t1 + t4P * t2 * t3 - t4P * t5 * t3 - t4P * t5 * t2 + t4P * t1 * t3 - t4P
         * t6 * t3 - t4P * t6 * t1 + t4P * t1 * t2 - t4P * t7 * t2 - t4P * t7 * t1 + t5 * t1P * t4 + t5 * t1P * t3 + t4P * t3 - t4P * t5 - t4P
         * t6 - t4P * t7 + t4P * t2 + t4P * t1 + t5 * t1P * t2 * t4 + t5 * t1P * t3 * t4 + t5 * t1P * t2 * t3 + t6 * t2P * t1 * t4 + t6 * t2P * t3
         * t4 + t6 * t2P * t1 * t3 + t7 * t3P * t2 * t4 + t7 * t3P * t1 * t2 + t7 * t3P * t1 * t4 - t4P * t7 * t1 * t2 - t4P * t6 * t2 * t3 - t4P * t6
         * t1 * t2 - t4P * t7 * t2 * t3 - t4P * t7 * t1 * t3 - t4P * t6 * t1 * t3 - t4P * t5 * t1 * t2 - t4P * t5 * t1 * t3 - t4P * t5 * t2 * t3 + t4P
         * t1 * t2 * t3 - t4P * t6 * t1 * t2 * t3 + t5 * t1P * t2 * t3 * t4 + t6 * t2P * t1 * t3 * t4 + t7 * t3P * t1 * t2 * t4 - t4P * t7 * t1 * t2 * t3), (-2));

         if (k > 0)
            test = Math.Abs(A11 / A[1, 1]);

         A[1, 1] += A11;
         k++;

      } while (test > Criteria && k < maxIter);

      if (k == maxIter)
      {
         iterFlag = 0;
         return;
      }

      //A12
      test = 100.0;
      k = 0;
      do
      {
         double t1P = Math.Pow((t1 / (1 + t1)), k);
         double t2P = Math.Pow((t2 / (1 + t2)), k);
         double t3P = Math.Pow((t3 / (1 + t3)), k);
         double t4P = Math.Pow((t4 / (1 + t4)), k);

         double A12 = 1 / (1 + t1) / (1 + t2) / (t7 * t3P + t6 * t2P + t5 * t1P + t4P - t4P * t5 * t1 * t2 * t3 + t5 * t1P * t2 + t6 * t2P * t4 + t6 * t2P
            * t3 + t6 * t2P * t1 + t7 * t3P * t4 + t7 * t3P * t2 + t7 * t3P * t1 - t4P * t6 * t2 - t4P * t7 * t3 - t4P * t5 * t1 + t4P * t2 * t3 - t4P
            * t5 * t3 - t4P * t5 * t2 + t4P * t1 * t3 - t4P * t6 * t3 - t4P * t6 * t1 + t4P * t1 * t2 - t4P * t7 * t2 - t4P * t7 * t1 + t5 * t1P * t4
            + t5 * t1P * t3 + t4P * t3 - t4P * t5 - t4P * t6 - t4P * t7 + t4P * t2 + t4P * t1 + t5 * t1P * t2 * t4 + t5 * t1P * t3 * t4 + t5 * t1P
            * t2 * t3 + t6 * t2P * t1 * t4 + t6 * t2P * t3 * t4 + t6 * t2P * t1 * t3 + t7 * t3P * t2 * t4 + t7 * t3P * t1 * t2 + t7 * t3P * t1 * t4 - t4P
            * t7 * t1 * t2 - t4P * t6 * t2 * t3 - t4P * t6 * t1 * t2 - t4P * t7 * t2 * t3 - t4P * t7 * t1 * t3 - t4P * t6 * t1 * t3 - t4P * t5 * t1 * t2 - t4P
            * t5 * t1 * t3 - t4P * t5 * t2 * t3 + t4P * t1 * t2 * t3 - t4P * t6 * t1 * t2 * t3 + t5 * t1P * t2 * t3 * t4 + t6 * t2P * t1 * t3 * t4 + t7 * t3P
            * t1 * t2 * t4 - t4P * t7 * t1 * t2 * t3) / t1 / t2 * (1 + t3) * (1 + t4) * t5 * t1P * t6 * t2P * (k * k - t2 * k - k * t1 + (t1 * t2));

         if (k > 0)
            test = Math.Abs(A12 / A[1, 2]);

         A[1, 2] += A12;
         k++;

      } while (test > Criteria && k < maxIter);


      if (k == maxIter)
      {
         iterFlag = 0;
         return;
      }

      //A13
      test = 100.0;
      k = 0;
      do
      {
         double t1P = Math.Pow((t1 / (1 + t1)), k);
         double t2P = Math.Pow((t2 / (1 + t2)), k);
         double t3P = Math.Pow((t3 / (1 + t3)), k);
         double t4P = Math.Pow((t4 / (1 + t4)), k);

         double A13 = 1 / (1 + t1) / (1 + t3) / (t7 * t3P + t6 * t2P + t5 * t1P + t4P - t4P * t5 * t1 * t2 * t3 + t5 * t1P * t2 + t6 * t2P * t4 + t6 * t2P
         * t3 + t6 * t2P * t1 + t7 * t3P * t4 + t7 * t3P * t2 + t7 * t3P * t1 - t4P * t6 * t2 - t4P * t7 * t3 - t4P * t5 * t1 + t4P * t2 * t3 - t4P
         * t5 * t3 - t4P * t5 * t2 + t4P * t1 * t3 - t4P * t6 * t3 - t4P * t6 * t1 + t4P * t1 * t2 - t4P * t7 * t2 - t4P * t7 * t1 + t5 * t1P * t4
         + t5 * t1P * t3 + t4P * t3 - t4P * t5 - t4P * t6 - t4P * t7 + t4P * t2 + t4P * t1 + t5 * t1P * t2 * t4 + t5 * t1P * t3 * t4 + t5 * t1P
         * t2 * t3 + t6 * t2P * t1 * t4 + t6 * t2P * t3 * t4 + t6 * t2P * t1 * t3 + t7 * t3P * t2 * t4 + t7 * t3P * t1 * t2 + t7 * t3P * t1 * t4 - t4P
         * t7 * t1 * t2 - t4P * t6 * t2 * t3 - t4P * t6 * t1 * t2 - t4P * t7 * t2 * t3 - t4P * t7 * t1 * t3 - t4P * t6 * t1 * t3 - t4P * t5 * t1 * t2 - t4P
         * t5 * t1 * t3 - t4P * t5 * t2 * t3 + t4P * t1 * t2 * t3 - t4P * t6 * t1 * t2 * t3 + t5 * t1P * t2 * t3 * t4 + t6 * t2P * t1 * t3 * t4 + t7 * t3P
         * t1 * t2 * t4 - t4P * t7 * t1 * t2 * t3) / t1 / t3 * (1 + t2) * (1 + t4) * t5 * t1P * t7 * t3P * (k * k - k * t3 - k * t1 + (t1 * t3));

         if (k > 0)
            test = Math.Abs(A13 / A[1, 3]);

         A[1, 3] += A13;
         k++;

      } while (test > Criteria && k < maxIter);


      if (k == maxIter)
      {
         iterFlag = 0;
         return;
      }

      //A14
      test = 100.0;
      k = 0;
      do
      {
         double t1P = Math.Pow((t1 / (1 + t1)), k);
         double t2P = Math.Pow((t2 / (1 + t2)), k);
         double t3P = Math.Pow((t3 / (1 + t3)), k);
         double t4P = Math.Pow((t4 / (1 + t4)), k);

         double A14 = -1 / (1 + t1) / (1 + t4) / (t7 * t3P + t6 * t2P + t5 * t1P + t4P - t4P * t5 * t1 * t2 * t3 + t5 * t1P * t2 + t6 * t2P * t4 + t6 * t2P
         * t3 + t6 * t2P * t1 + t7 * t3P * t4 + t7 * t3P * t2 + t7 * t3P * t1 - t4P * t6 * t2 - t4P * t7 * t3 - t4P * t5 * t1 + t4P * t2 * t3 - t4P
         * t5 * t3 - t4P * t5 * t2 + t4P * t1 * t3 - t4P * t6 * t3 - t4P * t6 * t1 + t4P * t1 * t2 - t4P * t7 * t2 - t4P * t7 * t1 + t5 * t1P * t4
         + t5 * t1P * t3 + t4P * t3 - t4P * t5 - t4P * t6 - t4P * t7 + t4P * t2 + t4P * t1 + t5 * t1P * t2 * t4 + t5 * t1P * t3 * t4 + t5 * t1P
         * t2 * t3 + t6 * t2P * t1 * t4 + t6 * t2P * t3 * t4 + t6 * t2P * t1 * t3 + t7 * t3P * t2 * t4 + t7 * t3P * t1 * t2 + t7 * t3P * t1 * t4 - t4P
         * t7 * t1 * t2 - t4P * t6 * t2 * t3 - t4P * t6 * t1 * t2 - t4P * t7 * t2 * t3 - t4P * t7 * t1 * t3 - t4P * t6 * t1 * t3 - t4P * t5 * t1 * t2 - t4P
         * t5 * t1 * t3 - t4P * t5 * t2 * t3 + t4P * t1 * t2 * t3 - t4P * t6 * t1 * t2 * t3 + t5 * t1P * t2 * t3 * t4 + t6 * t2P * t1 * t3 * t4 + t7 * t3P
         * t1 * t2 * t4 - t4P * t7 * t1 * t2 * t3) / t1 / t4 * (1 + t2) * (1 + t3) * t5 * t1P * t4P * (-k * k + t5 * k * k + k * k * t6 + k * k * t7 + k * t4 - t5 * k
         * t4 - k * t6 * t4 - k * t7 * t4 + k * t1 - t5 * k * t1 - k * t6 * t1 - k * t7 * t1 - (t1 * t4) + t5 * t1 * t4 + t6 * t1 * t4 + t7 * t1 * t4);

         if (k > 0)
            test = Math.Abs(A14 / A[1, 4]);

         A[1, 4] += A14;
         k++;

      } while (test > Criteria && k < maxIter);


      if (k == maxIter)
      {
         iterFlag = 0;
         return;
      }

      //A15
      test = 100.0;
      k = 0;
      do
      {
         double t1P = Math.Pow((t1 / (1 + t1)), k);
         double t2P = Math.Pow((t2 / (1 + t2)), k);
         double t3P = Math.Pow((t3 / (1 + t3)), k);
         double t4P = Math.Pow((t4 / (1 + t4)), k);

         double t12kP = Math.Pow((t1 / (1 + t1)), (2 * k));
         double t22kP = Math.Pow((t2 / (1 + t2)), (2 * k));
         double t32kP = Math.Pow((t3 / (1 + t3)), (2 * k));
         double t42kP = Math.Pow((t4 / (1 + t4)), (2 * k));

         double A15 = 1 / (1 + t1) * Math.Pow(t7 * t3P + t6 * t2P + t5 * t1P + t4P - t4P * t5 * t1 * t2 * t3 + t5 * t1P * t2 + t6 * t2P * t4 + t6 * t2P
         * t3 + t6 * t2P * t1 + t7 * t3P * t4 + t7 * t3P * t2 + t7 * t3P * t1 - t4P * t6 * t2 - t4P * t7 * t3 - t4P * t5 * t1 + t4P * t2 * t3 - t4P
         * t5 * t3 - t4P * t5 * t2 + t4P * t1 * t3 - t4P * t6 * t3 - t4P * t6 * t1 + t4P * t1 * t2 - t4P * t7 * t2 - t4P * t7 * t1 + t5 * t1P * t4
         + t5 * t1P * t3 + t4P * t3 - t4P * t5 - t4P * t6 - t4P * t7 + t4P * t2 + t4P * t1 + t5 * t1P * t2 * t4 + t5 * t1P * t3 * t4 + t5 * t1P
         * t2 * t3 + t6 * t2P * t1 * t4 + t6 * t2P * t3 * t4 + t6 * t2P * t1 * t3 + t7 * t3P * t2 * t4 + t7 * t3P * t1 * t2 + t7 * t3P * t1 * t4 - t4P
         * t7 * t1 * t2 - t4P * t6 * t2 * t3 - t4P * t6 * t1 * t2 - t4P * t7 * t2 * t3 - t4P * t7 * t1 * t3 - t4P * t6 * t1 * t3 - t4P * t5 * t1 * t2 - t4P
         * t5 * t1 * t3 - t4P * t5 * t2 * t3 + t4P * t1 * t2 * t3 - t4P * t6 * t1 * t2 * t3 + t5 * t1P * t2 * t3 * t4 + t6 * t2P * t1 * t3 * t4 + t7 * t3P
         * t1 * t2 * t4 - t4P * t7 * t1 * t2 * t3, -2) / t1 * (2 * t4P * t4 * t12kP * t1 * t5 * t3 + 8 * t42kP * t6 * t2 * t3 * t1P * k * t1 + 2 * t42kP
         * t3 * t5 * t1P * k * t1 * (t2 * t2) + 8 * t42kP * t7 * t2 * t1P * k * t3 - 2 * t42kP * t1P * t1 * t6 - 4 * t42kP * t2 * t3 * t1P * k + 2
         * t4P * t2 * t12kP * t1 * t5 * t4 - 2 * t4P * t7 * t3 * t4 * t12kP * t1 * t5 + 4 * t42kP * t6 * (t3 * t3) * t1P * t1 * t7 * t2 - t42kP
         * (t2 * t2) * t5 * t1P * k * t1 * t6 - t42kP * t1P * (t1 * t1) * t5 + t4P * t12kP * t1 * t5 + 2 * t6 * t6 * t22kP * t3 * t1P * (t1 * t1)
         + t6 * t6 * t22kP * (t4 * t4) * t1P * (t1 * t1) * (t3 * t3) + 4 * t42kP * t6 * t2 * t1P * t1 * t7 + t7 * t7 * t32kP * t1P * t1 * (t2 * t2) - 2
         * t42kP * t2 * (t3 * t3) * t5 * t1P * k * t6 + 2 * t4P * t2 * t1P * t1 * t6 * t2P * (t3 * t3) + t42kP * t6 * t6 * (t3 * t3) * t1P * t1
         * (t2 * t2) + 2 * t42kP * t7 * (t2 * t2) * t1P * k + 2 * t6 * t2P * (t3 * t3) * t4 * t12kP * t1 * t5 - 2 * t42kP * t7 * t3 * t5 * t1P * k
         * t1 * (t2 * t2) - 2 * t42kP * t7 * t1P * (t1 * t1) * (t2 * t2) + 2 * t42kP * t7 * (t2 * t2) * t1P * k * (t3 * t3) + t42kP * t7 * t7 * (t3
         * t3) * t1P * t1 - t42kP * t6 * t6 * t1P * k - 2 * t42kP * t2 * t1P * k - 4 * t4P * t6 * t2 * t1P * (t1 * t1) * t7 * t3P - 2 * t42kP
         * t7 * (t2 * t2) * t1P * k * t6 * t1 + 2 * t7 * t3P * t1P * t1 * t6 * t2P * (t4 * t4) - 2 * t7 * t3P * t1P * t1 * t4P * t6 * t4 - 2 * t7 * t7
         * t3P * t1P * t1 * t4P * t4 - 2 * t7 * t7 * t3P * t1P * t1 * t4P * (t2 * t2) - 2 * t6 * t6 * t2P * (t3 * t3) * t4 * t1P * t1 * t4P
         * t2 + 2 * t42kP * t6 * t1P * k * (t2 * t2) * (t3 * t3) - t42kP * t7 * (t2 * t2) * t1P * k * t5 - t42kP * (t2 * t2) * (t3 * t3) * t5 * t1P
         * k * t7 - t42kP * t5 * t1P * k * t6 + 2 * t42kP * t6 * (t3 * t3) * t1P * (t1 * t1) * t7 - 2 * t42kP * (t2 * t2) * t1P * t1 * t7 * (t3 * t3) - 2
         * t42kP * t6 * t6 * t1P * k * t1 * t2 + t7 * t7 * t32kP * t1P * t1 + 4 * t42kP * t2 * t3 * t1P * (t1 * t1) + t4P * t12kP * t1 * t5
         * (t3 * t3) + 4 * t7 * t7 * t32kP * t2 * t4 * t1P * (t1 * t1) - 2 * t42kP * t6 * t1P * (t1 * t1) - t42kP * t7 * (t3 * t3) * t5 * t1P * k
         * t1 + t42kP * t7 * t7 * t1P * t1 - 2 * t7 * t7 * t32kP * t2 * t1P * k + 4 * t7 * t3P * t2 * t3 * t4 * t12kP * t1 * t5 + 2 * t7 * t7 * t32kP
         * t1P * t1 * t4 - 2 * t42kP * t3 * t5 * t1P * k * t1 * t6 * (t2 * t2) - 4 * t42kP * t7 * t2 * t1P * k * t6 * (t3 * t3) + 2 * t42kP * t7 * t1P
         * (t1 * t1) * t5 * t2 - 2 * t42kP * (t2 * t2) * t3 * t5 * t1P * k * t7 + 2 * t7 * t7 * t32kP * t1P * t1 * t2 * (t4 * t4) + t42kP * (t2 * t2)
         * t1P * t1 - t42kP * t5 * t1P * k * t7 * (t3 * t3) + 2 * t42kP * t6 * (t2 * t2) * t1P * t1 * t7 - t42kP * t1P * k * t1 * (t2 * t2) - 2
         * t42kP * (t3 * t3) * t5 * t1P * k * t1 * t6 * t2 + 2 * t42kP * t7 * t7 * t1P * (t1 * t1) * t3 + 4 * t6 * t2P * t3 * t4 * t12kP * t1 * t5
         + t7 * t3P * t12kP * t1 * t5 * (t4 * t4) - 2 * t4P * t7 * t2 * t12kP * t1 * t5 * t4 + 2 * t7 * t3P * t12kP * t1 * t5 * t4 + 2 * t42kP
         * t5 * t1P * k * t3 + t4P * (t2 * t2) * t12kP * t1 * t5 + 4 * t4P * t2 * t12kP * t1 * t5 * t3 - t4P * t6 * t4 * t12kP * t1 * t5 * (t3
         * t3) - 4 * t6 * t6 * t22kP * t3 * t1P * k * t1 * t4 - t42kP * (t2 * t2) * (t3 * t3) * t5 * t1P * k * t6 - 2 * t4P * t6 * t2 * t12kP * t1
         * t5 - 4 * t42kP * t3 * t5 * t1P * k * t1 * t6 * t2 + 2 * t6 * t2P * (t3 * t3) * t4 * t12kP * t1 * t5 * t2 + 2 * t42kP * t6 * t1P * k * (t3
         * t3) - t4P * t7 * (t3 * t3) * t4 * t12kP * t1 * t5 - t42kP * t7 * (t3 * t3) * t5 * t1P * k * t1 * (t2 * t2) + 2 * t42kP * t6 * t3 * t1P
         * (t1 * t1) * t5 - 8 * t42kP * t2 * t1P * t1 * t7 * t3 - 2 * t42kP * t2 * t5 * t1P * k * t1 * t6 + 2 * t7 * t7 * t32kP * t1P * t1 * (t2 * t2)
         * t4 + 4 * t42kP * t7 * t2 * t1P * k * t1 - t7 * t7 * t32kP * t1P * k * t1 - t4P * t7 * (t2 * t2) * t12kP * t1 * t5 * (t3 * t3) - 2 * t42kP
         * t7 * t1P * (t1 * t1) - 4 * t42kP * t1P * t1 * t7 * t3 + 2 * t42kP * t6 * (t3 * t3) * t1P * t1 * t7 * (t2 * t2) + 4 * t42kP * t6 * t3 * t1P
         * t1 * t7 * (t2 * t2) - 4 * t42kP * t7 * t1P * (t1 * t1) * t2 - 4 * t42kP * t6 * (t2 * t2) * t1P * (t1 * t1) * t3 - 2 * t7 * t7 * t32kP * t2
         * t1P * k * t1 * (t4 * t4) - 4 * t42kP * t2 * t1P * t1 * t6 * (t3 * t3) - 4 * t42kP * t1P * t1 * t6 * t3 - 2 * t4P * t5 * t2 * t3 * t1P
         * t1 * t7 * t3P + 2 * t6 * t2P * t3 * (t4 * t4) * t1P * t1 * t7 * t3P - t42kP * t7 * t7 * t1P * k - 2 * t42kP * t7 * t7 * t2 * t1P * k
         * t1 * (t3 * t3) - 4 * t42kP * t2 * t3 * t1P * (t1 * t1) * t5 + 2 * t42kP * t5 * t1P * k * t2 - 2 * t6 * t6 * t22kP * (t3 * t3) * t1P * k
         * t1 * t4 + 2 * t4P * (t2 * t2) * t12kP * t1 * t5 * t3 * t4 - 2 * t6 * t2P * (t3 * t3) * t4 * t1P * t1 * t4P * t7 + 2 * t42kP * t3 * t5 * t1P
         * k * t1 + 2 * t7 * t7 * t32kP * t2 * (t4 * t4) * t1P * (t1 * t1) + t42kP * t6 * t6 * (t2 * t2) * t1P * t1 - 2 * t4P * t7 * t2 * t1P * t1
         * t6 * t2P + 4 * t7 * t3P * t2 * t3 * t4 * t1P * t1 * t4P - 2 * t6 * t2P * t3 * t4 * t1P * t1 * t4P * t5 - t42kP * t5 * t1P * t1 + 8
         * t42kP * t6 * t3 * t1P * t1 * t7 * t2 + 2 * t42kP * (t2 * t2) * t3 * t1P * (t1 * t1) - t7 * t3P * t3 * t4 * t5 * t1P * t4P * t1 - 2 * t6
         * t6 * t2P * (t3 * t3) * t4 * t1P * t1 * t4P - 4 * t42kP * t2 * t3 * t5 * t1P * k * t7 - 4 * t42kP * t7 * t2 * t1P * k * t6 * t1 - 2 * t42kP
         * t1P * t1 * t7 * (t3 * t3) - t7 * t7 * t32kP * t1P * k + 2 * t4P * t1P * t1 * t6 * t2P * (t3 * t3) - 2 * t6 * t6 * t22kP * t4 * t1P
         * k * (t3 * t3) - t42kP * t7 * t7 * t1P * k * t1 * (t3 * t3) - t4P * t6 * t4 * t12kP * t1 * t5 * (t2 * t2) + t42kP * t7 * t7 * t1P * (t1
         * t1) - t42kP * (t2 * t2) * t1P * k + t42kP * t1P * t1 - 2 * t7 * t3P * t1P * t1 * t4P * t6 - t7 * t3P * t1P * t1 * t4P * t5 - 2
         * t4P * t6 * t6 * (t3 * t3) * t1P * t1 * t2P - 4 * t4P * t6 * t6 * t3 * t1P * t1 * t2P - 2 * t7 * t7 * t3P * t1P * t1 * t4P * (t2
         * t2) * t3 + 4 * t6 * t2P * t3 * t4 * t1P * t1 * t7 * t3P - 2 * t7 * t3P * t2 * t3 * t4 * t1P * t1 * t4P * t5 - 4 * t7 * t3P * t2 * t3 * t4
         * t1P * t1 * t4P * t6 - 2 * t4P * t6 * t6 * t1P * t1 * t2P - 2 * t42kP * t1P * k * t1 * t2 - 4 * t42kP * t2 * t3 * t5 * t1P * k * t6
         + 2 * t42kP * t2 * (t3 * t3) * t5 * t1P * k + 4 * t42kP * t7 * t7 * t2 * t3 * t1P * (t1 * t1) - 2 * t42kP * t1P * (t1 * t1) * t5 * t3 + 2
         * t6 * t6 * t22kP * t4 * t1P * t1 - 4 * t7 * t7 * t3P * t1P * t1 * t4P * t2 - 2 * t7 * t3P * t1P * t1 * t4P * t6 * t3 - 2 * t7 * t3P
         * t1P * t1 * t4P * t5 * t2 - t7 * t3P * t1P * t1 * t4P * t5 * t3 - 2 * t7 * t7 * t3P * t1P * t1 * t4P * t3 - 8 * t42kP * t7 * t2 * t1P
         * k * t6 * t1 * t3 - t6 * t6 * t22kP * (t3 * t3) * t1P * k * t1 * (t4 * t4) + 4 * t42kP * t6 * t1P * k * t2 - 2 * t4P * t7 * t3 * t12kP * t1
         * t5 - 2 * t7 * t3P * t3 * t4 * t1P * t1 * t4P * t6 + t42kP * (t2 * t2) * t1P * (t1 * t1) + 2 * t4P * t4 * t1P * t1 * t6 * t2P + 2 * t4P
         * t4 * t1P * t1 * t7 * t3P - 4 * t6 * t6 * t2P * t2 * t3 * t1P * t1 * t4P - 2 * t6 * t2P * t2 * t3 * t1P * t1 * t4P * t5 - 4 * t7 * t7 * t3P
         * t1P * t1 * t4P * t2 * t4 - 2 * t42kP * t6 * (t3 * t3) * t1P * k * t1 * t7 - 2 * t42kP * t7 * t7 * (t2 * t2) * t1P * k * t1 * t3 - 2 * t42kP
         * t2 * t5 * t1P * t1 + t42kP * t7 * (t2 * t2) * t5 * t1P * t1 + 2 * t42kP * t7 * (t2 * t2) * t5 * t1P * t1 * t3 - t7 * t3P * t4 * t5 * t1P
         * t4P * t1 - 2 * t7 * t3P * t4 * t5 * t1P * t4P * t1 * t2 - 2 * t6 * t2P * t2 * t4 * t1P * t1 * t4P * t7 + 8 * t42kP * t6 * t2 * t1P
         * (t1 * t1) * t7 * t3 - t42kP * t5 * t1P * k * t6 * (t2 * t2) - 2 * t42kP * t7 * t7 * t2 * t1P * k * t1 - t7 * t7 * t32kP * (t2 * t2) * t1P
         * k * t1 + 2 * t42kP * t7 * (t2 * t2) * t3 * t1P * (t1 * t1) * t5 + t6 * t6 * t22kP * (t4 * t4) * t1P * t1 - 2 * t4P * t7 * t2 * (t3 * t3) * t4
         * t12kP * t1 * t5 - 4 * t42kP * t7 * t2 * (t3 * t3) * t1P * (t1 * t1) - 2 * t42kP * t6 * t6 * t1P * k * t2 * (t3 * t3) - 2 * t6 * t6 * t22kP
         * t4 * t1P * k - 2 * t4P * t6 * t3 * t12kP * t1 * t5 * (t2 * t2) + 2 * t7 * t3P * (t2 * t2) * t3 * t4 * t12kP * t1 * t5 - 4 * t6 * t6 * t2P
         * t3 * t4 * t1P * t1 * t4P * t2 + 2 * t6 * t2P * (t3 * t3) * t4 * t1P * t1 * t4P + 2 * t4P * (t2 * t2) * t1P * t1 * t7 * t3P * t3 + 4
         * t7 * t3P * t2 * t3 * t4 * t1P * t1 * t6 * t2P - 2 * t4P * t6 * t6 * t2 * t1P * t1 * t2P - 2 * t7 * t3P * (t2 * t2) * t3 * t4 * t1P * t1
         * t4P * t6 + t42kP * t6 * t5 * t1P * t1 - t42kP * (t3 * t3) * t1P * k - 2 * t5 * t12kP * k * t7 * t3P * t2 * t3 - 2 * t4P * t7 * t1P
         * (t1 * t1) * t6 * t2P * (t3 * t3) * t4 + 2 * t4P * t7 * t7 * t1P * k * t1 * t3P * t3 + 2 * t7 * t3P * (t4 * t4) * t1P * (t1 * t1) * t6 * t2P
         + 4 * t7 * t3P * t4 * t1P * (t1 * t1) * t6 * t2P - 2 * t6 * t2P * t3 * t1P * k * t1 * t7 * t3P + 4 * t4P * t4 * t1P * (t1 * t1) * t6 * t2P
         * t3 + 2 * t4P * t4 * t1P * (t1 * t1) * t6 * t2P + 2 * t4P * t4 * t1P * (t1 * t1) * t7 * t3P - 2 * t4P * t6 * t6 * (t3 * t3) * t1P * (t1
         * t1) * t2P - 4 * t4P * t6 * t6 * t3 * t1P * (t1 * t1) * t2P * t4 - 2 * t7 * t7 * t3P * (t2 * t2) * t4 * t1P * (t1 * t1) * t4P + 2 * t6
         * t6 * t2P * t4 * t1P * k * t4P * (t3 * t3) + 2 * t4P * t7 * t2 * t1P * k * t6 * t2P * (t3 * t3) * t4 + 2 * t4P * t7 * t7 * (t2 * t2) * t1P
         * k * t3P * t4 * t3 - 2 * t6 * t2P * t2 * t1P * (t1 * t1) * t4P * t7 * t4 - 4 * t4P * t6 * t6 * t3 * t1P * (t1 * t1) * t2P - 2 * t4P * t6
         * t3 * t1P * (t1 * t1) * t7 * t3P + 2 * t4P * t7 * t2 * t1P * k * t6 * t2P * t1 * (t3 * t3) + t4P * t5 * t1P * k * t7 * t3P * t3 + 2 * t4P
         * t6 * t3 * t1P * k * t1 * t7 * t3P + 4 * t6 * t2P * t3 * t1P * (t1 * t1) * t4P - 4 * t4P * t7 * t1P * (t1 * t1) * t6 * t2P * t3 * t4 - 2
         * t6 * t2P * t1P * k * t1 * t7 * t3P * (t4 * t4) - 4 * t5 * t12kP * t2 * t4 * k * t6 * t2P * t3 - t5 * t12kP * t2 * (t4 * t4) * k * t6 * t2P
         + 2 * t6 * t6 * t2P * t1P * k * t1 * t4P + t6 * t2P * t1P * k * t1 * t4P * t5 + 2 * t6 * t2P * (t3 * t3) * t1P * (t1 * t1) * t4P - t6
         * t2P * (t3 * t3) * t1P * (t1 * t1) * t4P * t5 + 2 * t7 * t3P * (t2 * t2) * t1P * k * t1 * t4P * t6 * t4 - 4 * t6 * t2P * t4 * t1P * k
         * t7 * t3P + 2 * t6 * t6 * t2P * t4 * t1P * k * t4P * t2 + 2 * t6 * t6 * t2P * t2 * t1P * k * t1 * t4P - 2 * t7 * t3P * (t2 * t2) * t1P
         * k * t4P - 4 * t7 * t3P * t2 * t1P * k * t4P - 2 * t6 * t2P * t1P * k * t4P - 4 * t4P * t7 * t2 * t3 * t1P * (t1 * t1) * t6 * t2P - 4
         * t4P * t7 * t7 * t2 * t3 * t1P * (t1 * t1) * t3P - 2 * t6 * t2P * (t4 * t4) * t1P * k * t7 * t3P * t2 - t4P * t4 * t5 * t1P * (t1 * t1)
         * t7 * t3P * t3 - t4P * t4 * t5 * t1P * (t1 * t1) * t6 * t2P * (t3 * t3) + t5 * t12kP * k * t4P * t6 * (t2 * t2) * (t3 * t3) - 2 * t7 * t7
         * t3P * t4 * t1P * (t1 * t1) * t4P * t3 + 2 * t42kP * t2 * t1P * t1 - 2 * t42kP * (t2 * t2) * t5 * t1P * t1 * t3 - 2 * t6 * t2P * (t3
         * t3) * t4 * t1P * t1 * t4P * t7 * t2 - 4 * t6 * t2P * t3 * t4 * t1P * t1 * t4P * t7 * t2 + 4 * t6 * t2P * t2 * t4 * t1P * t1 * t7 * t3P - 2
         * t6 * t2P * t3 * t4 * t1P * t1 * t4P * t5 * t2 - 2 * t42kP * t2 * (t3 * t3) * t5 * t1P * t1 - t42kP * (t2 * t2) * (t3 * t3) * t5 * t1P
         * t1 - 4 * t42kP * t2 * t1P * t1 * t7 + 2 * t42kP * (t2 * t2) * t1P * t1 * t3 + 2 * t7 * t7 * t32kP * (t2 * t2) * t4 * t1P * (t1 * t1) - 2
         * t7 * t7 * t3P * t4 * t1P * (t1 * t1) * t4P + 4 * t7 * t3P * t4 * t1P * (t1 * t1) * t6 * t2P * t3 + 2 * t4P * t6 * t6 * t2 * (t3 * t3)
         * t1P * k * t1 * t2P * t4 - 2 * t5 * t12kP * k * t6 * t2P * t3 * (t4 * t4) + 2 * t6 * t6 * t2P * (t3 * t3) * t1P * k * t1 * t4P + 2 * t6 * t2P
         * (t3 * t3) * t1P * k * t1 * t4P * t7 - t5 * t12kP * k * t6 * t2P * (t3 * t3) * t2 - 2 * t4P * t6 * t3 * t1P * (t1 * t1) * t7 * t3P * t4 - 2
         * t4P * t7 * t7 * t1P * (t1 * t1) * t3P * t3 - 2 * t4P * t7 * t7 * t1P * (t1 * t1) * t3P * (t2 * t2) + 2 * t6 * t6 * t2P * t2 * (t3 * t3)
         * t1P * k * t4P + 4 * t4P * t4 * t1P * (t1 * t1) * t7 * t3P * t2 - 4 * t5 * t12kP * t2 * t4 * k * t7 * t3P * t3 - 2 * t5 * t12kP * t2
         * t4 * k * t6 * t2P * (t3 * t3) - 2 * t4P * (t3 * t3) * t4 * t1P * k * t6 * t2P * t2 - 4 * t4P * t3 * t4 * t1P * k * t6 * t2P * t1 * t2 + 2
         * t4P * t4 * t5 * t1P * k * t1 * t7 * t3P * t2 * t3 + t4P * t4 * t5 * t1P * k * t1 * t6 * t2P * t2 + 2 * t4P * t7 * t2 * t1P * k * t6 * t2P
         * t1 * t4 - t5 * t12kP * k * t6 * t2P * (t4 * t4) + 2 * t4P * t7 * t7 * (t2 * t2) * t1P * k * t3P * t1 * t4 * t3 + 2 * t4P * t4 * t1P * (t1
         * t1) * t7 * t3P * (t2 * t2) * t3 + 4 * t5 * t12kP * k * t4P * t7 * t2 * t3 + t5 * t12kP * k * t4P * t6 * t4 + t5 * t12kP * k * t4P * t7
         * (t3 * t3) * t4 + 4 * t4P * t7 * t7 * t2 * t1P * k * t3P * t1 * t4 + t4P * t4 * t5 * t1P * k * t1 * t7 * t3P * (t2 * t2) * t3 + t4P * t4
         * t5 * t1P * k * t1 * t6 * t2P * (t3 * t3) * t2 + t4P * t4 * t5 * t1P * k * t1 * t6 * t2P * (t3 * t3) + t4P * t4 * t5 * t1P * k * t1 * t7
         * t3P * t3 + 4 * t4P * t4 * t1P * (t1 * t1) * t7 * t3P * t2 * t3 + 2 * t4P * t4 * t1P * (t1 * t1) * t7 * t3P * t3 + 2 * t4P * t4 * t1P
         * (t1 * t1) * t6 * t2P * (t3 * t3) + 2 * t4P * t4 * t1P * (t1 * t1) * t7 * t3P * (t2 * t2) + 2 * t4P * t4 * t1P * (t1 * t1) * t6 * t2P
         * t2 + 2 * t4P * t7 * t7 * (t2 * t2) * t1P * k * t3P * t1 + t7 * t7 * t32kP * t1P * t1 * (t2 * t2) * (t4 * t4) + 2 * t4P * t6 * t6 * t1P
         * k * t2P + 4 * t4P * t7 * t2 * t1P * k * t6 * t2P * t1 * t3 + 4 * t4P * t7 * t2 * t1P * k * t6 * t2P * t3 * t4 - 2 * t7 * t3P * t2 * t1P
         * k * t1 * t6 * t2P * t3 - 4 * t7 * t3P * t2 * t1P * k * t1 * t6 * t2P * t4 - 2 * t4P * t4 * t5 * t1P * (t1 * t1) * t6 * t2P * t3 + t5 * t12kP
         * k * t4P * t7 * t4 - 2 * t5 * t12kP * k * t7 * t3P * t4 * t3 - 2 * t6 * t2P * t3 * t1P * k * t7 * t3P - 2 * t6 * t2P * (t3 * t3) * t1P
         * k * t4P + 2 * t4P * t6 * t1P * k * t7 * t3P + 2 * t5 * t12kP * t2 * t4 * k * t4P * t6 - 2 * t4P * t6 * t6 * t2 * t1P * (t1 * t1) * t2P
         + 4 * t4P * t4 * t1P * (t1 * t1) * t6 * t2P * t3 * t2 + 2 * t6 * t2P * t2 * t1P * (t1 * t1) * t7 * t3P + 4 * t6 * t2P * t2 * t1P * (t1
         * t1) * t4P * t3 + 2 * t6 * t6 * t2P * t2 * t1P * k * t1 * t4P * t4 - 4 * t4P * t7 * t1P * (t1 * t1) * t6 * t2P * t3 - 2 * t4P * t7 * t1P
         * (t1 * t1) * t6 * t2P * t4 - 2 * t4P * t7 * t1P * (t1 * t1) * t6 * t2P - 2 * t4P * t7 * t7 * t1P * (t1 * t1) * t3P + 4 * t6 * t2P * t3
         * t1P * k * t4P * t7 + 4 * t6 * t6 * t2P * t3 * t1P * k * t4P - t4P * (t2 * t2) * t4 * t5 * t1P * (t1 * t1) * t7 * t3P * t3 + 2 * t5 * t12kP
         * t2 * t4 * k * t4P * t7 * (t3 * t3) + t5 * t12kP * (t2 * t2) * t4 * k * t4P * t7 * (t3 * t3) - t5 * t12kP * k * t4P * (t3 * t3) + 2 * t4P
         * t7 * t7 * (t2 * t2) * t1P * k * t3P * t1 * t3 + 2 * t5 * t12kP * k * t4P * t7 * t2 * (t3 * t3) + t4P * t4 * t5 * t1P * k * t7 * t3P + t4P
         * t4 * t5 * t1P * k * t6 * t2P + 2 * t4P * t7 * t2 * t1P * k * t6 * t2P * t1 + 2 * t4P * t4 * t5 * t1P * k * t6 * t2P * t3 + t4P * t4
         * t5 * t1P * k * t6 * t2P * t2 + t4P * t4 * t5 * t1P * k * t7 * t3P * (t2 * t2) + t4P * t4 * t5 * t1P * k * t6 * t2P * (t3 * t3) + 4 * t4P
         * t7 * t2 * t1P * k * t6 * t2P * t3 + 2 * t4P * t7 * t2 * t1P * k * t6 * t2P * t4 + 2 * t4P * t6 * (t2 * t2) * t3 * t1P * k * t1 * t7 * t3P
         * t4 + t4P * t4 * t5 * t1P * k * t7 * t3P * t3 + 2 * t5 * t12kP * k * t4P * t7 * t3 * t4 + 2 * t4P * t4 * t5 * t1P * k * t7 * t3P * t2 + t5
         * t12kP * k * t4P * t7 * (t2 * t2) * (t3 * t3) - 2 * t4P * (t3 * t3) * t4 * t1P * k * t6 * t2P * t1 + 4 * t4P * t7 * t7 * t2 * t1P * k * t3P - 2
         * t7 * t3P * t2 * t1P * k * t1 * t6 * t2P * t3 * (t4 * t4) + 2 * t4P * t7 * t2 * t1P * k * t6 * t2P - 2 * t4P * t4 * t1P * k * t6 * t2P
         * t1 + t5 * t12kP * k * t4P * t6 * (t3 * t3) * t4 - 4 * t7 * t3P * t2 * t1P * k * t1 * t6 * t2P * t3 * t4 - 2 * t4P * t6 * t6 * t1P * (t1
         * t1) * t2P * t4 + 2 * t5 * t12kP * k * t4P * t6 * t3 * t4 + 2 * t6 * t2P * t1P * k * t1 * t4P * t7 * t4 + 2 * t4P * t6 * t1P * k * t1 * t7
         * t3P * t4 + 4 * t4P * t7 * t7 * t2 * t1P * k * t3P * t1 + 2 * t4P * t7 * t7 * (t2 * t2) * t1P * k * t3P + 4 * t4P * t7 * t7 * t2 * t1P
         * k * t3P * t4 - t5 * t12kP * k * t7 * t3P * (t4 * t4) * t3 + 2 * t4P * t6 * t6 * t2 * (t3 * t3) * t1P * k * t1 * t2P + 4 * t4P * t6 * t6
         * t2 * t3 * t1P * k * t1 * t2P * t4 + 4 * t4P * t7 * t7 * t2 * t1P * k * t3P * t1 * t3 + 4 * t4P * t7 * t7 * t2 * t1P * k * t3P * t4 * t3
         + 2 * t4P * t7 * t7 * (t2 * t2) * t1P * k * t3P * t3 + 2 * t6 * t2P * t4 * t12kP * t1 * t5 - 2 * t42kP * (t2 * t2) * t3 * t1P * k + 2 * t6
         * t2P * t2 * t3 * t1P * t1 * t7 * t3P + 2 * t4P * t2 * t1P * t1 * t6 * t2P + 2 * t4P * t4 * t1P * t1 * t7 * t3P * t3 - 2 * t6 * t2P
         * t2 * t1P * k * t4P - 4 * t4P * t3 * t4 * t1P * k * t6 * t2P - 2 * t4P * t3 * t4 * t1P * k * t7 * t3P + 2 * t7 * t7 * t3P * t3 * t1P
         * k * t4P + 2 * t6 * t2P * t3 * t1P * k * t4P * t5 + 2 * t4P * t6 * t1P * k * t1 * t7 * t3P - 4 * t7 * t7 * t3P * t2 * t4 * t1P * (t1
         * t1) * t4P + 4 * t6 * t2P * t3 * t1P * k * t1 * t4P * t7 + 2 * t4P * t7 * t7 * t4 * t1P * k * t3P + 2 * t4P * t7 * t7 * t3 * t4 * t1P
         * k * t3P + 4 * t4P * t6 * t6 * t3 * t4 * t1P * k * t1 * t2P + 2 * t4P * t6 * t3 * t4 * t1P * k * t1 * t7 * t3P + 2 * t4P * t6 * t6 * (t3
         * t3) * t4 * t1P * k * t1 * t2P - 2 * t5 * t12kP * k * t6 * t2P * (t3 * t3) * t4 - 4 * t7 * t3P * t2 * t1P * k * t1 * t4P + 2 * t6 * t6 * t2P
         * t1P * k * t1 * t4P * t4 - 4 * t4P * t1P * k * t1 * t6 * t2P * t3 + 2 * t4P * t7 * t2 * t1P * k * t6 * t2P * t1 * (t3 * t3) * t4 - t5 * t12kP
         * k * t6 * t2P * (t3 * t3) * (t4 * t4) + 4 * t5 * t12kP * k * t4P * t6 * t2 * t3 - 2 * t4P * t1P * k * t1 * t6 * t2P - 2 * t4P * t1P * k
         * t1 * t7 * t3P - 2 * t4P * t3 * t4 * t1P * k * t7 * t3P * (t2 * t2) - 4 * t4P * t3 * t4 * t1P * k * t6 * t2P * t2 - 4 * t6 * t2P * t1P
         * k * t1 * t7 * t3P * t4 + 2 * t5 * t12kP * k * t4P * t6 * (t2 * t2) * t3 - 4 * t5 * t12kP * k * t6 * t2P * t3 * t4 + t4P * (t3 * t3) * t5 * t1P
         * k * t1 * t6 * t2P + 2 * t4P * t3 * t5 * t1P * k * t1 * t6 * t2P + t4P * t3 * t5 * t1P * k * t1 * t7 * t3P + 2 * t6 * t2P * t2 * t1P
         * (t1 * t1) * t7 * t3P * (t4 * t4) + 2 * t6 * t2P * (t3 * t3) * t1P * k * t1 * t4P * t7 * t4 - 2 * t6 * t2P * t2 * t3 * t1P * k * t7 * t3P
         * (t4 * t4) - 4 * t4P * t6 * t6 * t2 * t1P * (t1 * t1) * t2P * t3 * t4 - 4 * t4P * t2 * t3 * t1P * k * t7 * t3P - t4P * t4 * t5 * t1P
         * (t1 * t1) * t6 * t2P - t4P * t4 * t5 * t1P * (t1 * t1) * t7 * t3P - 2 * t5 * t12kP * k * t6 * t2P * t4 - 4 * t4P * t6 * t6 * t2 * t1P
         * (t1 * t1) * t2P * t3 - 2 * t4P * t6 * t6 * t2 * t1P * (t1 * t1) * t2P * t4 + 2 * t4P * t4 * t5 * t1P * k * t1 * t6 * t2P * t3 + t4P
         * t4 * t5 * t1P * k * t1 * t6 * t2P - t5 * t12kP * k * t4P + 2 * t42kP * t7 * t2 * (t3 * t3) * t1P * (t1 * t1) * t5 - 4 * t42kP * t6 * t2
         * t1P * (t1 * t1) * (t3 * t3) + t6 * t2P * t2 * (t3 * t3) * t12kP * t1 * t5 - t42kP * t5 * t1P * t1 * (t3 * t3) + 2 * t42kP * t6 * t6
         * (t3 * t3) * t1P * t1 * t2 + 2 * t7 * t3P * t12kP * t1 * t5 * t2 * (t4 * t4) - 2 * t42kP * t7 * t7 * (t2 * t2) * t1P * k * t3 + 2 * t42kP
         * t6 * (t2 * t2) * t1P * (t1 * t1) * t7 + t7 * t7 * t32kP * (t4 * t4) * t1P * (t1 * t1) - t6 * t6 * t22kP * (t4 * t4) * t1P * k * (t3 * t3)
         + t42kP * t7 * t7 * t1P * (t1 * t1) * (t3 * t3) - 2 * t42kP * t6 * (t2 * t2) * t1P * (t1 * t1) * (t3 * t3) + 2 * t4P * t1P * (t1 * t1)
         * t6 * t2P + t6 * t6 * t22kP * (t3 * t3) * t1P * t1 - t42kP * t5 * t1P * k * t6 * (t3 * t3) + 2 * t6 * t6 * t22kP * (t4 * t4) * t1P
         * (t1 * t1) * t3 - t42kP * (t2 * t2) * (t3 * t3) * t1P * (t1 * t1) * t5 + t6 * t6 * t22kP * t1P * (t1 * t1) + 2 * t42kP * t6 * t1P * k
         + t42kP * t6 * (t2 * t2) * t1P * (t1 * t1) * t5 + t7 * t3P * (t2 * t2) * t3 * (t4 * t4) * t12kP * t1 * t5 + 2 * t7 * t7 * t32kP * t1P
         * t1 * t2 - 2 * t42kP * t6 * t6 * (t2 * t2) * t3 * t1P * k * t1 - 2 * t6 * t2P * t3 * t1P * (t1 * t1) * t4P * t5 - 2 * t4P * t7 * t1P * (t1
         * t1) * t6 * t2P * (t3 * t3) - t42kP * (t2 * t2) * (t3 * t3) * t1P * k * t1 + 4 * t4P * t7 * t7 * t2 * t1P * k * t3P * t3 + 4 * t4P * t6
         * t6 * t2 * t3 * t1P * k * t1 * t2P - 2 * t7 * t3P * (t2 * t2) * t1P * k * t1 * t4P * t4 - 4 * t7 * t3P * t2 * t1P * k * t1 * t4P * t4 - 2
         * t4P * t4 * t1P * k * t6 * t2P - 2 * t4P * t4 * t1P * k * t7 * t3P + 2 * t4P * t7 * t2 * t1P * k * t6 * t2P * (t3 * t3) - 2 * t5 * t12kP
         * k * t6 * t2P * t3 * t2 - 4 * t4P * t3 * t4 * t1P * k * t7 * t3P * t1 * t2 + 4 * t4P * t6 * t1P * k * t7 * t3P * t2 + 4 * t4P * t6 * t2 * t3
         * t1P * k * t1 * t7 * t3P - 2 * t6 * t2P * t1P * k * t1 * t7 * t3P + 2 * t4P * t6 * t1P * k * t7 * t3P * t3 + 2 * t4P * t6 * t1P * k
         * t7 * t3P * t4 + t4P * t4 * t5 * t1P * k * t1 * t7 * t3P * (t2 * t2) + 2 * t4P * t4 * t5 * t1P * k * t1 * t6 * t2P * t3 * t2 + 4 * t7 * t3P
         * t2 * t1P * k * t1 * t4P * t6 * t4 - 2 * t5 * t12kP * t2 * t4 * k * t6 * t2P - 4 * t5 * t12kP * t2 * t4 * k * t7 * t3P + t5 * t12kP * (t2
         * t2) * t4 * k * t4P * t6 - 4 * t5 * t12kP * t2 * t4 * k * t4P * t3 + t4P * t5 * t1P * k * t6 * t2P + t4P * t5 * t1P * k * t7 * t3P - 2
         * t4P * t7 * t7 * (t2 * t2) * t3 * t1P * (t1 * t1) * t3P * t4 - 2 * t4P * t4 * t1P * k * t7 * t3P * t1 - 4 * t4P * t4 * t1P * k * t7 * t3P
         * t2 + t4P * t5 * t1P * k * t7 * t3P * t1 + 2 * t4P * t5 * t1P * k * t7 * t3P * t2 - 2 * t4P * t6 * t6 * (t3 * t3) * t1P * (t1 * t1) * t2P
         * t4 + 2 * t4P * t3 * t5 * t1P * k * t1 * t6 * t2P * t2 + t4P * t3 * t5 * t1P * k * t1 * t7 * t3P * (t2 * t2) - 2 * t6 * t2P * t2 * t3 * t1P
         * k * t7 * t3P - 2 * t4P * t3 * t4 * t1P * k * t7 * t3P * t1 - 4 * t4P * t3 * t4 * t1P * k * t7 * t3P * t2 + t4P * (t3 * t3) * t5 * t1P
         * k * t1 * t6 * t2P * t2 + t6 * t2P * t2 * t1P * k * t4P * t5 - t4P * t2 * t4 * t5 * t1P * (t1 * t1) * t6 * t2P - 2 * t4P * t2 * t4 * t5
         * t1P * (t1 * t1) * t7 * t3P - t4P * (t2 * t2) * t4 * t5 * t1P * (t1 * t1) * t7 * t3P + t4P * t2 * (t3 * t3) * t5 * t1P * k * t6 * t2P
         * t4 + t4P * (t2 * t2) * t3 * t5 * t1P * k * t7 * t3P * t4 + 4 * t4P * t6 * t2 * t3 * t1P * k * t1 * t7 * t3P * t4 - 2 * t4P * t2 * t4 * t5
         * t1P * (t1 * t1) * t6 * t2P * t3 + 2 * t4P * t6 * (t2 * t2) * t3 * t1P * k * t1 * t7 * t3P - 2 * t7 * t3P * t2 * t1P * k * t1 * t6 * t2P
         * (t4 * t4) - 2 * t6 * t2P * t3 * t1P * k * t1 * t7 * t3P * (t4 * t4) + t4P * (t2 * t2) * t3 * t5 * t1P * k * t7 * t3P + 2 * t4P * t2 * t3
         * t5 * t1P * k * t7 * t3P * t4 - 2 * t4P * t4 * t1P * k * t7 * t3P * (t2 * t2) - 2 * t42kP * t3 * t5 * t1P * k * t1 * t6 - 2 * t42kP * t6
         * t6 * t2 * (t3 * t3) * t1P * k * t1 - t42kP * t6 * t6 * t1P * k * (t3 * t3) + 2 * t42kP * t1P * k * t1 * t7 - 2 * t6 * t6 * t22kP * t3 * t1P
         * k - 2 * t6 * t6 * t22kP * t3 * t1P * k * t1 * (t4 * t4) - 2 * t42kP * t7 * t7 * t2 * t1P * k * (t3 * t3) - t42kP * t7 * t7 * t1P * k * (t3
         * t3) + 4 * t42kP * t6 * t2 * t1P * (t1 * t1) * t5 * t3 + t6 * t2P * (t3 * t3) * t12kP * t1 * t5 + t42kP * t7 * (t2 * t2) * (t3 * t3) * t1P
         * (t1 * t1) * t5 + t42kP * t6 * t6 * (t3 * t3) * t1P * t1 - 4 * t42kP * (t2 * t2) * t1P * t1 * t7 * t3 + 4 * t42kP * t6 * t2 * t1P * (t1
         * t1) * t7 - 2 * t4P * t6 * t4 * t12kP * t1 * t5 * t2 * (t3 * t3) + 4 * t6 * t6 * t22kP * t3 * t4 * t1P * t1 + t6 * t6 * t22kP * (t3 * t3) * (t4
         * t4) * t1P * t1 + 4 * t42kP * t6 * t3 * t1P * t1 * t7 - t42kP * t1P * k - t6 * t2P * t5 * t1P * t4P * t1 * t4 - t6 * t2P * t5 * t1P
         * t4P * t1 - 2 * t6 * t2P * t5 * t1P * t4P * t1 * t3 - 2 * t42kP * t5 * t1P * t1 * t3 - 4 * t4P * t7 * t3 * t1P * t1 * t6 * t2P - 4
         * t42kP * t2 * t1P * t1 * t6 + 2 * t42kP * t7 * t3 * t5 * t1P * t1 + t42kP * t7 * (t3 * t3) * t5 * t1P * t1 - 2 * t7 * t3P * t1P * t1
         * t4P * t6 * (t2 * t2) * t3 - 2 * t4P * t7 * t4 * t1P * t1 * t6 * t2P + 2 * t6 * t2P * t2 * t4 * t12kP * t1 * t5 + 2 * t7 * t3P * t12kP
         * t1 * t5 * t2 - 4 * t6 * t2P * t2 * t3 * t1P * t1 * t4P * t7 + 4 * t7 * t3P * t12kP * t1 * t5 * t2 * t4 + t6 * t2P * t2 * t12kP * t1 * t5
         + t7 * t3P * t12kP * t1 * t5 * t3 + t42kP * t7 * t7 * (t2 * t2) * t1P * t1 * (t3 * t3) + 2 * t42kP * t2 * t1P * t1 * (t3 * t3) - 2 * t7
         * t7 * t3P * t1P * t1 * t4P * (t2 * t2) * t4 + 2 * t7 * t3P * t1P * t1 * t6 * t2P * t2 + t7 * t3P * t3 * (t4 * t4) * t12kP * t1 * t5 - 2
         * t42kP * t7 * t7 * t1P * k * t3 + t42kP * (t3 * t3) * t5 * t1P * k * t1 * (t2 * t2) - 4 * t4P * t7 * t2 * t12kP * t1 * t5 * t3 - 2 * t42kP
         * t7 * (t3 * t3) * t5 * t1P * k * t1 * t2 + 2 * t42kP * t6 * t1P * k * (t2 * t2) + 2 * t42kP * t7 * (t2 * t2) * t1P * k * t1 + 2 * t42kP * t6
         * (t2 * t2) * t1P * (t1 * t1) * t5 * t3 - 2 * t7 * t7 * t32kP * t4 * t1P * k - 2 * t42kP * t2 * t1P * (t1 * t1) * t5 - t42kP * t1P * k
         * t1 * (t3 * t3) - t7 * t7 * t32kP * (t4 * t4) * t1P * k * (t2 * t2) + 8 * t42kP * t6 * t1P * k * t2 * t3 - 2 * t42kP * t7 * t7 * t2 * t1P
         * k + t42kP * t7 * t1P * (t1 * t1) * t5 * (t3 * t3) + 2 * t42kP * t1P * k * t1 * t6 + 2 * t7 * t3P * t12kP * t1 * t5 * t2 * t3 + 2 * t7 * t7
         * t32kP * t4 * t1P * (t1 * t1) - 4 * t42kP * t7 * t1P * (t1 * t1) * t3 - t42kP * t7 * t7 * (t2 * t2) * t1P * k * (t3 * t3) + 2 * t42kP
         * t7 * t7 * (t2 * t2) * t1P * t1 * t3 + 2 * t42kP * t6 * t6 * t2 * t1P * (t1 * t1) * (t3 * t3) - 2 * t42kP * (t2 * t2) * t3 * t5 * t1P * k * t6 - 2
         * t42kP * t7 * (t2 * t2) * (t3 * t3) * t1P * (t1 * t1) + 2 * t42kP * t7 * (t2 * t2) * t1P * k * t1 * (t3 * t3) + 2 * t6 * t2P * t3 * t12kP
         * t1 * t5 - 8 * t42kP * t7 * t2 * t3 * t1P * (t1 * t1) + t7 * t7 * t32kP * (t2 * t2) * (t4 * t4) * t1P * (t1 * t1) + 4 * t4P * t2 * t1P
         * t1 * t7 * t3P * t3 - t7 * t7 * t32kP * (t2 * t2) * t1P * k * t1 * (t4 * t4) + 4 * t42kP * t2 * t3 * t5 * t1P * k - 4 * t4P * t6 * t4 * t12kP
         * t1 * t5 * t2 * t3 + 4 * t42kP * t6 * t1P * k * (t2 * t2) * t3 + 2 * t42kP * (t3 * t3) * t1P * k * t7 - 2 * t7 * t3P * t1P * t1 * t4P * t6
         * (t2 * t2) * t4 + 4 * t4P * t2 * t1P * t1 * t6 * t2P * t3 * t4 - 2 * t4P * t6 * t6 * t4 * t1P * t1 * t2P + 2 * t4P * (t2 * t2) * t1P
         * t1 * t7 * t3P * t4 + 2 * t7 * t7 * t32kP * t1P * (t1 * t1) * t2 + t42kP * t7 * t1P * (t1 * t1) * t5 * (t2 * t2) + 2 * t42kP * t6 * t2
         * t1P * (t1 * t1) * t5 - 8 * t42kP * t7 * t2 * t1P * k * t6 * t3 - 4 * t42kP * t6 * t1P * k * t7 * t3 + t6 * t6 * t22kP * t1P * t1 - 2 * t42kP
         * t2 * (t3 * t3) * t1P * k + t42kP * t1P * k * t1 * t5 - 2 * t42kP * t7 * t2 * t1P * k * t5 + 2 * t42kP * t6 * (t2 * t2) * (t3 * t3) * t1P
         * k * t1 - 4 * t42kP * (t2 * t2) * t1P * t1 * t6 * t3 + t42kP * t6 * (t3 * t3) * t1P * (t1 * t1) * t5 - 2 * t42kP * t7 * t3 * t5 * t1P * k
         * t1 + t7 * t3P * t12kP * t1 * t5 * (t2 * t2) * (t4 * t4) + 4 * t42kP * t6 * t2 * (t3 * t3) * t1P * k * t1 - 2 * t6 * t6 * t22kP * t3 * t1P
         * k * t1 + 4 * t42kP * t1P * k * t1 * t6 * t3 + 2 * t42kP * t7 * t1P * t1 * t6 - 2 * t42kP * t6 * t6 * t1P * k * (t2 * t2) * t3 + 2 * t42kP
         * t7 * t7 * t2 * (t3 * t3) * t1P * (t1 * t1) - 4 * t42kP * t7 * (t2 * t2) * t3 * t1P * (t1 * t1) + t42kP * t7 * t1P * (t1 * t1) * t5 - t42kP
         * t7 * t7 * (t2 * t2) * t1P * k * t1 + t42kP * t6 * t6 * (t2 * t2) * t1P * (t1 * t1) + 2 * t7 * t3P * t2 * t3 * (t4 * t4) * t1P * t1 * t6 * t2P
         + 2 * t7 * t3P * (t2 * t2) * t3 * t4 * t1P * t1 * t4P + t42kP * (t2 * t2) * (t3 * t3) * t5 * t1P * k + 2 * t42kP * t6 * t6 * t3 * t1P
         * t1 * (t2 * t2) + 2 * t42kP * t6 * t3 * t5 * t1P * t1 + 4 * t42kP * t6 * t3 * t5 * t1P * t1 * t2 + 4 * t42kP * t1P * k * t1 * t7 * t3 + t6
         * t2P * (t3 * t3) * (t4 * t4) * t12kP * t1 * t5 * t2 - 4 * t7 * t3P * t1P * t1 * t4P * t6 * t2 * t4 + 2 * t4P * t2 * t1P * t1 * t6 * t2P
         * (t3 * t3) * t4 - t42kP * t6 * t6 * t1P * k * t1 * (t2 * t2) - t42kP * t7 * t7 * (t2 * t2) * t1P * k * t1 * (t3 * t3) + t4P * t4 * t5 * t1P
         * k * t1 * t7 * t3P - 2 * t4P * (t2 * t2) * t3 * t1P * k * t7 * t3P - t6 * t2P * t2 * t1P * (t1 * t1) * t4P * t5 * (t3 * t3) - 2 * t7 * t3P
         * t2 * t1P * k * t1 * t6 * t2P - 2 * t4P * (t3 * t3) * t4 * t1P * k * t6 * t2P * t1 * t2 - 2 * t4P * t6 * (t2 * t2) * t1P * (t1 * t1) * t7
         * t3P * t4 + 2 * t5 * t12kP * t2 * t4 * k * t4P * t6 * (t3 * t3) - t5 * t12kP * (t2 * t2) * (t4 * t4) * k * t7 * t3P * t3 + t5 * t12kP * (t2
         * t2) * t4 * k * t4P * t6 * (t3 * t3) - t5 * t12kP * t2 * (t4 * t4) * k * t6 * t2P * (t3 * t3) + 2 * t6 * t6 * t2P * t2 * (t3 * t3) * t1P * k
         * t4P * t4 - 2 * t4P * t4 * t1P * k * t6 * t2P * t2 + 2 * t6 * t2P * t4 * t1P * k * t4P * t7 + 2 * t4P * t7 * t7 * t1P * k * t3P + 2
         * t4P * t4 * t1P * (t1 * t1) * t6 * t2P * (t3 * t3) * t2 - 4 * t4P * t2 * t3 * t1P * k * t1 * t6 * t2P + 4 * t4P * t7 * t7 * t2 * t1P
         * k * t3P * t1 * t4 * t3 - t5 * t12kP * k * t7 * t3P * (t2 * t2) * t3 - 2 * t4P * t1P * k * t1 * t7 * t3P * t3 - 2 * t4P * t1P * k * t1
         * t6 * t2P * (t3 * t3) - 2 * t6 * t2P * (t4 * t4) * t1P * k * t7 * t3P * t3 - 4 * t6 * t2P * t4 * t1P * k * t7 * t3P * t3 + t5 * t12kP
         * (t2 * t2) * t4 * k * t4P * t7 + 4 * t5 * t12kP * t2 * t4 * k * t4P * t6 * t3 + 4 * t5 * t12kP * t2 * t4 * k * t4P * t7 * t3 - 2 * t5 * t12kP
         * t2 * t4 * k * t4P * (t3 * t3) - 2 * t5 * t12kP * t2 * t4 * k * t4P - 2 * t4P * t7 * t2 * (t3 * t3) * t1P * (t1 * t1) * t6 * t2P - 4 * t4P
         * t7 * t2 * t3 * t1P * (t1 * t1) * t6 * t2P * t4 - 2 * t5 * t12kP * t2 * (t4 * t4) * k * t6 * t2P * t3 + 2 * t5 * t12kP * (t2 * t2) * t4 * k * t4P
         * t6 * t3 - t5 * t12kP * (t2 * t2) * (t4 * t4) * k * t7 * t3P + 2 * t5 * t12kP * (t2 * t2) * t4 * k * t4P * t7 * t3 + 4 * t6 * t6 * t2P * t3 * t1P
         * k * t1 * t4P - 2 * t4P * t7 * t2 * (t3 * t3) * t1P * (t1 * t1) * t6 * t2P * t4 + 4 * t4P * t6 * t2 * t3 * t4 * t1P * k * t7 * t3P + 2 * t5
         * t12kP * k * t4P * t6 * t2 + 2 * t5 * t12kP * t2 * t4 * k * t4P * t7 + 4 * t6 * t6 * t2P * t2 * t3 * t1P * k * t4P * t4 + 4 * t6 * t6 * t2P
         * t4 * t1P * k * t4P * t3 + 4 * t6 * t2P * t4 * t1P * k * t4P * t7 * t3 + 4 * t6 * t6 * t2P * t2 * t3 * t1P * k * t4P + 2 * t6 * t6 * t2P
         * (t3 * t3) * t1P * k * t4P + t6 * t2P * (t3 * t3) * t1P * k * t4P * t5 + 2 * t6 * t2P * (t3 * t3) * t1P * k * t4P * t7 - t5 * t12kP
         * k * t4P * (t2 * t2) + 4 * t6 * t2P * t3 * t1P * k * t1 * t4P * t7 * t4 + 4 * t4P * t2 * t3 * t1P * (t1 * t1) * t7 * t3P + 2 * t4P * (t2
         * t2) * t3 * t1P * (t1 * t1) * t7 * t3P + 2 * t4P * t7 * t7 * t1P * k * t1 * t3P + t4P * t5 * t1P * k * t7 * t3P * (t2 * t2) + 2 * t7
         * t3P * t1P * (t1 * t1) * t6 * t2P + 2 * t7 * t3P * t1P * (t1 * t1) * t4P * t3 - t7 * t3P * t1P * (t1 * t1) * t4P * t5 * (t2 * t2)
         * t3 - t7 * t3P * t1P * (t1 * t1) * t4P * t5 * (t2 * t2) - 2 * t7 * t3P * t1P * (t1 * t1) * t4P * t5 * t2 - t7 * t3P * t1P * (t1 * t1)
         * t4P * t5 * t3 - 4 * t6 * t2P * t3 * t1P * k * t4P - 2 * t7 * t3P * (t2 * t2) * t1P * k * t1 * t4P * t3 - 2 * t4P * t1P * k * t1 * t6
         * t2P * t2 - 4 * t4P * t3 * t4 * t1P * k * t6 * t2P * t1 - 2 * t4P * (t3 * t3) * t4 * t1P * k * t6 * t2P + 2 * t6 * t2P * t4 * t1P * k
         * t4P * t7 * (t3 * t3) + 2 * t6 * t6 * t2P * t2 * t1P * k * t4P + 4 * t6 * t2P * t2 * t1P * (t1 * t1) * t7 * t3P * t4 + 2 * t6 * t2P * t2
         * t1P * (t1 * t1) * t7 * t3P * (t4 * t4) * t3 + 4 * t6 * t2P * t2 * t1P * (t1 * t1) * t7 * t3P * t4 * t3 + 2 * t6 * t2P * t2 * t1P * (t1
         * t1) * t7 * t3P * t3 - 2 * t6 * t2P * t2 * t1P * (t1 * t1) * t4P * t5 * t3 + 2 * t6 * t2P * t2 * t1P * (t1 * t1) * t4P * (t3 * t3) + 2
         * t6 * t2P * t2 * t1P * (t1 * t1) * t4P - 2 * t4P * t6 * (t2 * t2) * t1P * (t1 * t1) * t7 * t3P - 4 * t4P * t6 * t2 * t1P * (t1 * t1)
         * t7 * t3P * t4 - 4 * t5 * t12kP * k * t4P * t2 * t3 - 2 * t4P * t7 * t7 * (t2 * t2) * t3 * t1P * (t1 * t1) * t3P - 4 * t4P * t7 * t7 * t2
         * t3 * t1P * (t1 * t1) * t3P * t4 + 2 * t6 * t2P * t1P * k * t4P * t7 - 2 * t7 * t3P * t1P * (t1 * t1) * t4P * t5 * t2 * t3 - 2 * t4P
         * t3 * t1P * k * t7 * t3P - 2 * t6 * t2P * t2 * t1P * k * t7 * t3P - 4 * t4P * t6 * t2 * t1P * (t1 * t1) * t7 * t3P * t4 * t3 - 2 * t4P
         * t6 * (t2 * t2) * t1P * (t1 * t1) * t7 * t3P * t3 - 2 * t7 * t3P * (t2 * t2) * t1P * k * t1 * t4P + 2 * t6 * t2P * t1P * k * t1 * t4P
         * t7 - 2 * t5 * t12kP * k * t4P * t2 - t5 * t12kP * k * t4P * t4 - 2 * t5 * t12kP * k * t4P * t3 - t5 * t12kP * k * t7 * t3P - t5 * t12kP
         * k * t6 * t2P + t5 * t12kP * k * t4P * t6 + t4P * t2 * (t3 * t3) * t5 * t1P * k * t6 * t2P + 2 * t4P * t2 * t3 * t5 * t1P * k * t6 * t2P
         * t4 + 2 * t4P * t2 * t3 * t5 * t1P * k * t6 * t2P + 2 * t4P * t2 * t3 * t5 * t1P * k * t7 * t3P + 2 * t4P * t6 * t1P * k * t7 * t3P * (t2
         * t2) + 2 * t5 * t12kP * k * t4P * t6 * t3 + 2 * t5 * t12kP * k * t4P * t7 * t2 + 4 * t4P * t6 * t1P * k * t7 * t3P * t2 * t3 + 2 * t4P
         * t6 * t1P * k * t7 * t3P * t4 * t3 - t5 * t12kP * k * t6 * t2P * t2 + 2 * t4P * t6 * t1P * k * t7 * t3P * (t2 * t2) * t4 + t5 * t12kP
         * k * t4P * t6 * (t2 * t2) - 2 * t5 * t12kP * k * t4P * (t2 * t2) * t3 - t5 * t12kP * k * t7 * t3P * (t2 * t2) + t5 * t12kP * k * t4P
         * t7 * (t2 * t2) - t5 * t12kP * k * t6 * t2P * (t3 * t3) - t5 * t12kP * k * t7 * t3P * t3 + 2 * t4P * t6 * t1P * k * t7 * t3P * (t2 * t2)
         * t3 - 2 * t5 * t12kP * k * t4P * t2 * (t3 * t3) + 2 * t4P * t4 * t5 * t1P * k * t1 * t7 * t3P * t2 - t5 * t12kP * k * t4P * (t2 * t2) * (t3
         * t3) + t5 * t12kP * k * t4P * t6 * (t3 * t3) + t5 * t12kP * k * t4P * t7 * (t3 * t3) + 2 * t4P * t3 * t5 * t1P * k * t1 * t7 * t3P * t2 - 2
         * t5 * t12kP * k * t4P * t3 * t4 - t5 * t12kP * k * t7 * t3P * (t4 * t4) - 2 * t5 * t12kP * k * t7 * t3P * t4 - 2 * t5 * t12kP * k * t7
         * t3P * t2 + 2 * t5 * t12kP * k * t4P * t7 * t3 + t4P * (t2 * t2) * t5 * t1P * k * t1 * t7 * t3P + 4 * t4P * t6 * t1P * k * t7 * t3P
         * t2 * t4 - 2 * t4P * t6 * t6 * t2 * t1P * (t1 * t1) * t2P * (t3 * t3) - 4 * t4P * t6 * t2 * t1P * (t1 * t1) * t7 * t3P * t3 + 2 * t6 * t6 * t2P
         * t4 * t1P * k * t4P + t5 * t12kP * k * t4P * t7 + 2 * t4P * t6 * (t2 * t2) * t3 * t4 * t1P * k * t7 * t3P + 2 * t7 * t3P * t1P * (t1
         * t1) * t4P - 2 * t6 * t2P * t1P * k * t7 * t3P - 2 * t6 * t2P * t2 * t1P * (t1 * t1) * t4P * t7 - t6 * t2P * t2 * t1P * (t1 * t1)
         * t4P * t5 + 2 * t7 * t7 * t3P * t4 * t1P * k * t1 * t4P * t3 - t7 * t3P * t1P * (t1 * t1) * t4P * t5 + 2 * t7 * t3P * t1P * (t1 * t1)
         * t6 * t2P * t3 + 2 * t7 * t3P * (t4 * t4) * t1P * (t1 * t1) * t6 * t2P * t3 + 4 * t4P * t2 * t1P * (t1 * t1) * t7 * t3P + 2 * t4P * (t2
         * t2) * t1P * (t1 * t1) * t7 * t3P - 2 * t4P * t6 * t1P * (t1 * t1) * t7 * t3P * t4 + 2 * t5 * t12kP * k * t4P * t7 * (t2 * t2) * t3 - 4
         * t6 * t2P * t3 * t1P * k * t1 * t7 * t3P * t4 + 4 * t7 * t3P * t2 * t1P * k * t1 * t4P * t6 - 4 * t4P * t7 * t7 * t1P * (t1 * t1) * t3P
         * t2 - t42kP * t6 * t6 * t1P * k * (t2 * t2) * (t3 * t3) - 2 * t6 * t6 * t2P * t2 * (t3 * t3) * t1P * t1 * t4P - t6 * t2P * t2 * (t3 * t3)
         * t1P * t1 * t4P * t5 - 2 * t6 * t2P * t2 * (t3 * t3) * t1P * t1 * t4P * t7 - 2 * t4P * t7 * t1P * t1 * t6 * t2P + 4 * t4P * t1P
         * t1 * t6 * t2P * t3 - 2 * t6 * t6 * t2P * t2 * t4 * t1P * t1 * t4P + 4 * t42kP * t3 * t5 * t1P * k * t1 * t2 - 2 * t4P * t6 * (t3 * t3) * t12kP
         * t1 * t5 * t2 + t6 * t2P * t12kP * t1 * t5 - 2 * t42kP * t7 * (t2 * t2) * t1P * k * t6 * t1 * (t3 * t3) - 4 * t42kP * t6 * t6 * t1P * k * t2
         * t3 - 2 * t42kP * (t2 * t2) * t3 * t1P * (t1 * t1) * t5 + 2 * t6 * t2P * t2 * (t4 * t4) * t1P * t1 * t7 * t3P + 4 * t42kP * t7 * t2 * t1P
         * k * (t3 * t3) - t4P * t7 * (t2 * t2) * (t3 * t3) * t4 * t12kP * t1 * t5 - 4 * t4P * t6 * t3 * t12kP * t1 * t5 * t2 - 2 * t42kP * (t2 * t2)
         * t1P * t1 * t6 + 4 * t42kP * t7 * t2 * t1P * k * t1 * (t3 * t3) - t42kP * t5 * t1P * k * t7 + 2 * t42kP * t6 * t6 * t2 * t1P * t1 + t42kP
         * t5 * t1P * k * (t2 * t2) - t42kP * t5 * t1P * k * t7 * t1 + 2 * t7 * t3P * t12kP * t1 * t5 * (t2 * t2) * t4 - 4 * t7 * t7 * t3P * t1P
         * t1 * t4P * t2 * t3 + 4 * t4P * t2 * t1P * t1 * t6 * t2P * t3 - t7 * t3P * (t2 * t2) * t5 * t1P * t4P * t1 + 2 * t4P * t2 * t1P * t1
         * t6 * t2P * t4 - 2 * t7 * t7 * t3P * t3 * t4 * t1P * t1 * t4P + 2 * t42kP * t6 * t2 * t5 * t1P * t1 + t42kP * t6 * (t2 * t2) * t5 * t1P
         * t1 - t6 * t2P * t2 * t4 * t1P * t1 * t4P * t5 - t42kP * (t2 * t2) * t5 * t1P * t1 + t42kP * t6 * t6 * t1P * (t1 * t1) - 4 * t42kP
         * t6 * t6 * t2 * t3 * t1P * k * t1 - 2 * t4P * t6 * t4 * t12kP * t1 * t5 * t3 + 8 * t42kP * t7 * t2 * t1P * k * t1 * t3 - 2 * t7 * t7 * t32kP
         * (t4 * t4) * t1P * k * t2 - t42kP * (t2 * t2) * t5 * t1P * k * t1 * t7 + 2 * t42kP * t7 * t2 * t5 * t1P * t1 + 4 * t42kP * t7 * t2 * t5 * t1P
         * t1 * t3 + t6 * t6 * t22kP * (t3 * t3) * t1P * (t1 * t1) + t6 * t2P * (t3 * t3) * (t4 * t4) * t12kP * t1 * t5 + 4 * t42kP * t1P * k * t1
         * t6 * t2 - 2 * t42kP * t6 * t1P * k * t1 * t7 + t42kP * (t2 * t2) * (t3 * t3) * t1P * (t1 * t1) - t6 * t6 * t22kP * (t4 * t4) * t1P * k
         + t42kP * t1P * (t1 * t1) - 4 * t42kP * t2 * t1P * t1 * t7 * (t3 * t3) + 2 * t42kP * t7 * t1P * (t1 * t1) * t5 * t3 + 2 * t7 * t3P * t2
         * t3 * (t4 * t4) * t12kP * t1 * t5 + t7 * t7 * t32kP * t1P * (t1 * t1) * (t2 * t2) - 2 * t4P * t7 * (t2 * t2) * t12kP * t1 * t5 * t3 - 2 * t42kP
         * t6 * t6 * t3 * t1P * k * t1 + 4 * t42kP * t7 * (t2 * t2) * t1P * k * t3 + 2 * t42kP * t2 * t5 * t1P * k * t1 + 2 * t42kP * t7 * t2 * (t3 * t3)
         * t5 * t1P * t1 + t42kP * t7 * (t2 * t2) * (t3 * t3) * t5 * t1P * t1 - 2 * t7 * t7 * t3P * t1P * t1 * t4P - 2 * t7 * t3P * t1P * t1
         * t4P * t6 * (t2 * t2) + 4 * t4P * t2 * t1P * t1 * t7 * t3P * t4 - 2 * t7 * t7 * t3P * (t2 * t2) * t3 * t4 * t1P * t1 * t4P - 4 * t6 * t6
         * t2P * t3 * t4 * t1P * t1 * t4P - 2 * t5 * t12kP * (t2 * t2) * t4 * k * t4P * t3 - 2 * t5 * t12kP * (t2 * t2) * t4 * k * t7 * t3P - 2 * t5
         * t12kP * t2 * (t4 * t4) * k * t7 * t3P - t5 * t12kP * (t2 * t2) * t4 * k * t4P - 2 * t5 * t12kP * t2 * (t4 * t4) * k * t7 * t3P * t3 - 2
         * t5 * t12kP * (t2 * t2) * t4 * k * t7 * t3P * t3 - t5 * t12kP * (t2 * t2) * t4 * k * t4P * (t3 * t3) + 2 * t7 * t3P * t3 * t4 * t12kP * t1
         * t5 - 2 * t4P * t6 * t6 * t1P * (t1 * t1) * t2P - 2 * t4P * t6 * (t2 * t2) * t1P * (t1 * t1) * t7 * t3P * t4 * t3 - 2 * t4P * t6 * t6 * t2
         * t1P * (t1 * t1) * t2P * (t3 * t3) * t4 - 2 * t6 * t2P * t2 * (t3 * t3) * t1P * k * t4P - 4 * t6 * t2P * t2 * t3 * t1P * k * t7 * t3P
         * t4 - 4 * t6 * t2P * t4 * t1P * k * t7 * t3P * t2 - 2 * t6 * t2P * (t4 * t4) * t1P * k * t7 * t3P - 2 * t4P * t4 * t1P * k * t6 * t2P
         * t1 * t2 + 2 * t7 * t7 * t3P * t4 * t1P * k * t1 * t4P - 2 * t4P * t6 * t1P * (t1 * t1) * t7 * t3P + 2 * t5 * t12kP * k * t4P * t6 * t2
         * (t3 * t3) + 2 * t4P * t7 * t7 * (t2 * t2) * t1P * k * t3P * t4 - 2 * t4P * t3 * t4 * t1P * k * t7 * t3P * t1 * (t2 * t2) - t5 * t12kP
         * k * t4P * (t3 * t3) * t4 + t4P * t2 * t5 * t1P * k * t1 * t6 * t2P + 2 * t4P * t2 * t5 * t1P * k * t1 * t7 * t3P - 4 * t6 * t2P * t2 * t3
         * t1P * k * t4P + 2 * t4P * t7 * t7 * (t2 * t2) * t1P * k * t3P * t1 * t4 - 2 * t5 * t12kP * k * t6 * t2P * t3 + 4 * t4P * t7 * t2 * t1P
         * k * t6 * t2P * t1 * t3 * t4 - t4P * t2 * t4 * t5 * t1P * (t1 * t1) * t6 * t2P * (t3 * t3) - 2 * t4P * t2 * t4 * t5 * t1P * (t1 * t1) * t7
         * t3P * t3 - t6 * t2P * t1P * (t1 * t1) * t4P * t5 - 2 * t4P * t2 * (t3 * t3) * t1P * k * t1 * t6 * t2P + 2 * t7 * t3P * (t2 * t2) * t1P
         * k * t1 * t4P * t6 - 4 * t7 * t3P * t2 * t1P * k * t1 * t4P * t3 - 2 * t4P * t1P * k * t7 * t3P - 2 * t42kP * t5 * t1P * k * t6 * t3
         + 2 * t4P * (t2 * t2) * t1P * t1 * t7 * t3P + t42kP * t6 * t6 * (t2 * t2) * t1P * (t1 * t1) * (t3 * t3) + 2 * t7 * t3P * t1P * t1 * t6
         * t2P * t3 + 4 * t7 * t3P * t1P * t1 * t6 * t2P * t4 + 2 * t7 * t3P * t1P * t1 * t6 * t2P - 4 * t7 * t3P * t1P * t1 * t4P * t6 * t2 - t7
         * t3P * (t2 * t2) * t3 * t4 * t1P * t1 * t4P * t5 - t6 * t6 * t22kP * (t3 * t3) * t1P * k - t42kP * t5 * t1P * k * t6 * t1 + 2 * t6 * t6
         * t22kP * (t3 * t3) * t4 * t1P * t1 + 4 * t42kP * t7 * t2 * t1P * k - 2 * t4P * t7 * t2 * t12kP * t1 * t5 * (t3 * t3) - t4P * t6 * t4
         * t12kP * t1 * t5 - t42kP * t6 * t6 * (t3 * t3) * t1P * k * t1 + 4 * t7 * t7 * t32kP * t1P * t1 * t2 * t4 - 2 * t42kP * t2 * (t3 * t3) * t5
         * t1P * k * t7 + 2 * t6 * t6 * t22kP * t4 * t1P * (t1 * t1) + 2 * t42kP * t6 * t6 * t3 * t1P * t1 + t7 * t3P * t12kP * t1 * t5 * (t2 * t2)
         * t3 + 2 * t4P * t2 * t12kP * t1 * t5 * (t3 * t3) * t4 + 4 * t42kP * t6 * t1P * k * t2 * (t3 * t3) - t6 * t6 * t22kP * t1P * k * t1 * (t4
         * t4) + t6 * t2P * (t4 * t4) * t12kP * t1 * t5 - 4 * t42kP * t7 * t2 * t1P * k * t6 - t42kP * t6 * t6 * (t2 * t2) * (t3 * t3) * t1P * k
         * t1 + t7 * t3P * t12kP * t1 * t5 * (t2 * t2) + 2 * t6 * t2P * t2 * t3 * t12kP * t1 * t5 - t4P * t7 * (t2 * t2) * t12kP * t1 * t5 + 4 * t4P
         * t2 * t1P * t1 * t7 * t3P + 4 * t42kP * t7 * t7 * t2 * t1P * t1 * t3 - 4 * t42kP * t6 * t2 * t1P * (t1 * t1) + 2 * t4P * (t2 * t2) * t12kP
         * t1 * t5 * t3 - 2 * t42kP * (t2 * t2) * t1P * t1 * t7 + t4P * t4 * t12kP * t1 * t5 + 4 * t42kP * t6 * t6 * t3 * t1P * t1 * t2 + 2 * t4P
         * t12kP * t1 * t5 * t3 - 4 * t42kP * t7 * t7 * t2 * t1P * k * t1 * t3 - 2 * t7 * t7 * t32kP * t4 * t1P * k * (t2 * t2) - t7 * t7 * t32kP * (t4
         * t4) * t1P * k - 2 * t7 * t7 * t32kP * (t2 * t2) * t1P * k * t1 * t4 - 4 * t6 * t6 * t22kP * t4 * t1P * k * t3 - 4 * t7 * t7 * t32kP * t4 * t1P
         * k * t2 - t4P * t7 * t12kP * t1 * t5 - 2 * t42kP * t6 * t1P * k * t7 * (t3 * t3) + t7 * t3P * t12kP * t1 * t5 - 4 * t6 * t2P * t3 * t4
         * t1P * t1 * t4P * t7 + 2 * t42kP * t6 * t3 * t5 * t1P * t1 * (t2 * t2) + t42kP * t6 * (t3 * t3) * t5 * t1P * t1 * (t2 * t2) - t6 * t2P
         * (t3 * t3) * t5 * t1P * t4P * t1 * t4 - t6 * t2P * (t3 * t3) * t5 * t1P * t4P * t1 - 4 * t7 * t7 * t3P * t2 * t3 * t4 * t1P * t1 * t4P
         + t42kP * t6 * (t3 * t3) * t5 * t1P * t1 + 2 * t42kP * t6 * (t3 * t3) * t5 * t1P * t1 * t2 - t7 * t3P * (t2 * t2) * t5 * t1P * t4P * t1
         * t4 - 4 * t42kP * t7 * t7 * t2 * t1P * k * t3 + 2 * t42kP * t7 * t7 * t2 * t1P * t1 * (t3 * t3) + 2 * t6 * t6 * t22kP * t3 * t1P * t1 - t4P
         * t6 * (t2 * t2) * t12kP * t1 * t5 + t7 * t7 * t32kP * t1P * t1 * (t4 * t4) + 2 * t42kP * (t3 * t3) * t5 * t1P * k * t1 * t2 + 2 * t42kP
         * t7 * t7 * t3 * t1P * t1 + 4 * t42kP * t6 * (t2 * t2) * t1P * (t1 * t1) * t7 * t3 + 2 * t42kP * t1P * k * t1 * t7 * (t3 * t3) + 2 * t6 * t6 * t22kP
         * t4 * t1P * (t1 * t1) * (t3 * t3) + 2 * t42kP * t6 * (t3 * t3) * t1P * t1 * t7 - 2 * t4P * t7 * (t2 * t2) * t3 * t4 * t12kP * t1 * t5 - 2 * t42kP
         * t6 * t6 * t1P * k * t2 - 2 * t42kP * t2 * t5 * t1P * k * t1 * t7 - 2 * t42kP * t1P * k * t1 * t3 + 4 * t42kP * t6 * t2 * t1P * (t1 * t1)
         * t7 * (t3 * t3) + 2 * t42kP * t7 * t1P * k + 4 * t6 * t6 * t22kP * t4 * t1P * (t1 * t1) * t3 + 4 * t42kP * t6 * t3 * t1P * (t1 * t1) * t7
         + 2 * t42kP * t6 * t6 * (t2 * t2) * t1P * (t1 * t1) * t3 + 4 * t42kP * t6 * t1P * k * t3 + t6 * t6 * t22kP * (t4 * t4) * t1P * (t1 * t1) - t4P
         * t7 * (t2 * t2) * t12kP * t1 * t5 * t4 - 2 * t4P * t7 * t2 * t12kP * t1 * t5 - 2 * t6 * t6 * t22kP * t1P * k * t1 * t4 - 2 * t42kP * t6 * (t2
         * t2) * t1P * (t1 * t1) - 8 * t42kP * t2 * t1P * t1 * t6 * t3 - 2 * t42kP * t1P * t1 * t7 + t42kP * t6 * t6 * t1P * t1 + 2 * t6 * t6 * t22kP
         * t3 * (t4 * t4) * t1P * t1 - 4 * t42kP * t2 * t3 * t1P * k * t1 - t42kP * t7 * t7 * t1P * k * t1 - t42kP * (t2 * t2) * t1P * (t1 * t1)
         * t5 - 2 * t7 * t7 * t32kP * t4 * t1P * k * t1 + 4 * t42kP * t6 * t6 * t2 * t1P * (t1 * t1) * t3 - 4 * t42kP * t6 * t3 * t1P * k * t1 * t7 + t7
         * t7 * t32kP * t1P * (t1 * t1) + 2 * t4P * t2 * t12kP * t1 * t5 * (t3 * t3) - t6 * t6 * t22kP * (t3 * t3) * t1P * k * t1 - 2 * t42kP
         * (t2 * t2) * t1P * t1 * t6 * (t3 * t3) + t42kP * t5 * t1P * k * (t3 * t3) - t4P * t7 * (t3 * t3) * t12kP * t1 * t5 - 2 * t42kP * t2 * (t3
         * t3) * t1P * (t1 * t1) * t5 + 2 * t42kP * t1P * (t1 * t1) * t3 - t6 * t6 * t22kP * t1P * k + 2 * t42kP * t6 * (t2 * t2) * t1P * (t1
         * t1) * t7 * (t3 * t3) - t42kP * t7 * t7 * (t2 * t2) * t1P * k - t4P * t6 * (t3 * t3) * t12kP * t1 * t5 + 2 * t42kP * t7 * t7 * t1P * (t1
         * t1) * t2 + 4 * t42kP * t6 * (t2 * t2) * t3 * t1P * k * t1 - 2 * t4P * t7 * (t3 * t3) * t1P * t1 * t6 * t2P + 2 * t4P * t1P * t1 * t6 * t2P
         + 2 * t4P * t1P * t1 * t7 * t3P + 2 * t42kP * t6 * t2 * t1P * (t1 * t1) * t5 * (t3 * t3) - 2 * t4P * t6 * t3 * t12kP * t1 * t5 + 4 * t42kP
         * t7 * (t2 * t2) * t1P * k * t1 * t3 + 2 * t42kP * t1P * t1 * t3 - 2 * t42kP * t6 * t6 * t1P * k * t3 + t42kP * t5 * t1P * k + 2 * t42kP
         * t7 * t7 * (t2 * t2) * t3 * t1P * (t1 * t1) - 2 * t42kP * t7 * t7 * t1P * k * t1 * t3 + t42kP * t7 * t7 * (t2 * t2) * (t3 * t3) * t1P * (t1
         * t1) + 2 * t4P * t1P * t1 * t7 * t3P * t3 - 4 * t42kP * t2 * t5 * t1P * t1 * t3 + 2 * t42kP * t6 * t6 * t3 * t1P * (t1 * t1) - 4 * t7 * t7
         * t32kP * t2 * t1P * k * t1 * t4 - 2 * t7 * t7 * t32kP * t2 * t1P * k * t1 - t4P * t6 * (t3 * t3) * t12kP * t1 * t5 * (t2 * t2) - t4P
         * t6 * t12kP * t1 * t5 - 2 * t42kP * (t2 * t2) * t3 * t1P * k * t1 + t42kP * t7 * t7 * t1P * (t1 * t1) * (t2 * t2) - 2 * t42kP * t2 * (t3
         * t3) * t1P * k * t1 - 4 * t42kP * t6 * t3 * t1P * (t1 * t1) + 2 * t6 * t2P * t3 * (t4 * t4) * t12kP * t1 * t5 * t2 - 2 * t42kP * t7 * (t2
         * t2) * t1P * k * t6 * (t3 * t3) + t42kP * t6 * (t2 * t2) * t1P * (t1 * t1) * t5 * (t3 * t3) + 2 * t42kP * t1P * k * t1 * t6 * (t2 * t2) - 2
         * t42kP * t5 * t1P * k * t7 * t3 - 2 * t42kP * t3 * t1P * k - 2 * t42kP * t7 * (t2 * t2) * t1P * k * t6 - 4 * t4P * t7 * t2 * t3 * t4 * t12kP
         * t1 * t5 - 2 * t6 * t6 * t22kP * (t4 * t4) * t1P * k * t3 + t42kP * t7 * t5 * t1P * t1 - t4P * t5 * (t2 * t2) * t3 * t1P * t1 * t7 * t3P - t4P
         * t7 * t4 * t12kP * t1 * t5 + 2 * t42kP * t7 * t7 * t2 * t1P * t1 + t42kP * t6 * t1P * (t1 * t1) * t5 - t4P * t6 * t4 * t12kP * t1 * t5
         * (t2 * t2) * (t3 * t3) - 2 * t42kP * t5 * t1P * k * t6 * t2 + 4 * t4P * t2 * t12kP * t1 * t5 * t3 * t4 - 2 * t42kP * t7 * t1P * (t1 * t1)
         * (t3 * t3) - 2 * t4P * t6 * t4 * t12kP * t1 * t5 * (t2 * t2) * t3 + t4P * (t2 * t2) * t12kP * t1 * t5 * (t3 * t3) + 4 * t42kP * t7 * t2 * t3
         * t1P * (t1 * t1) * t5 + t6 * t2P * t2 * (t4 * t4) * t12kP * t1 * t5 - t42kP * t1P * k * t1 + 2 * t42kP * t1P * k * t1 * t6 * (t3 * t3)
         + 4 * t6 * t2P * t3 * t4 * t12kP * t1 * t5 * t2 + t42kP * (t3 * t3) * t5 * t1P * k * t1 - t7 * t7 * t32kP * (t4 * t4) * t1P * k * t1 + t42kP
         * t6 * t6 * (t3 * t3) * t1P * (t1 * t1) + 2 * t4P * t2 * t12kP * t1 * t5 + 2 * t42kP * t6 * t1P * (t1 * t1) * t7 - 2 * t42kP * t6 * t1P
         * k * t7 - 2 * t42kP * t1P * t1 * t6 * (t3 * t3) + t42kP * (t2 * t2) * t1P * t1 * (t3 * t3) - 4 * t42kP * t7 * t3 * t5 * t1P * k * t1 * t2
         + t42kP * t1P * t1 * (t3 * t3) - t42kP * t6 * t6 * t1P * k * (t2 * t2) - 2 * t4P * t6 * t4 * t12kP * t1 * t5 * t2 - 2 * t42kP * t6 * (t3
         * t3) * t1P * (t1 * t1) + 4 * t42kP * t3 * t1P * k * t7 - t6 * t2P * (t3 * t3) * t4 * t1P * t1 * t4P * t5 * t2 - t4P * t5 * t2 * t1P
         * t1 * t6 * t2P + 2 * t42kP * t6 * t6 * t2 * t1P * (t1 * t1) + 2 * t42kP * t2 * (t3 * t3) * t1P * (t1 * t1) + 2 * t42kP * (t2 * t2) * t3
         * t5 * t1P * k - 4 * t42kP * t7 * t2 * t1P * k * t6 * t1 * (t3 * t3) - 4 * t42kP * t7 * (t2 * t2) * t1P * k * t6 * t1 * t3 - 4 * t42kP * t7
         * (t2 * t2) * t1P * k * t6 * t3 + t42kP * t7 * t7 * (t2 * t2) * t1P * t1 + t42kP * t1P * (t1 * t1) * (t3 * t3) - t7 * t7 * t32kP * (t2
         * t2) * t1P * k + 4 * t42kP * t2 * t1P * t1 * t3 + t4P * (t2 * t2) * t12kP * t1 * t5 * (t3 * t3) * t4 - t42kP * (t2 * t2) * (t3 * t3)
         * t1P * k + t4P * t4 * t12kP * t1 * t5 * (t3 * t3) - t42kP * t6 * t6 * t1P * k * t1 - t6 * t6 * t22kP * t1P * k * t1 - t42kP * (t3
         * t3) * t5 * t1P * k * t1 * t6 - 8 * t42kP * t6 * t2 * t1P * (t1 * t1) * t3 + 2 * t6 * t2P * t3 * (t4 * t4) * t12kP * t1 * t5 + t42kP * (t2
         * t2) * t5 * t1P * k * t1 + 4 * t6 * t2P * t3 * t4 * t1P * t1 * t4P - 4 * t7 * t3P * t1P * t1 * t4P * t6 * t2 * t3 + 2 * t42kP * t2 * t1P
         * (t1 * t1) - t42kP * (t3 * t3) * t5 * t1P * k * t1 * t6 * (t2 * t2) - t42kP * t1P * (t1 * t1) * t5 * (t3 * t3) + t4P * (t2 * t2) * t12kP
         * t1 * t5 * t4);

         if (k > 0)
            test = Math.Abs(A15 / A[1, 5]);

         A[1, 5] += A15;
         k++;

      } while (test > Criteria && k < maxIter);


      if (k == maxIter)
      {
         iterFlag = 0;
         return;
      }

      //A16
      test = 100.0;
      k = 0;
      do
      {
         double t1P = Math.Pow((t1 / (1 + t1)), k);
         double t2P = Math.Pow((t2 / (1 + t2)), k);
         double t3P = Math.Pow((t3 / (1 + t3)), k);
         double t4P = Math.Pow((t4 / (1 + t4)), k);

         double A16 = -(1 + t3) * t5 * t1P * (-k * t2P - k * t2P * t4 + t4P * k + t4P * k * t2 + t2P * t1 + t2P * t1 * t4 - t4P * t1 - t4P * t1 * t2) / (1
         + t1) / (t7 * t3P + t6 * t2P + t5 * t1P + t4P - t4P * t5 * t1 * t2 * t3 + t5 * t1P * t2 + t6 * t2P * t4 + t6 * t2P * t3 + t6 * t2P
         * t1 + t7 * t3P * t4 + t7 * t3P * t2 + t7 * t3P * t1 - t4P * t6 * t2 - t4P * t7 * t3 - t4P * t5 * t1 + t4P * t2 * t3 - t4P * t5 * t3 - t4P
         * t5 * t2 + t4P * t1 * t3 - t4P * t6 * t3 - t4P * t6 * t1 + t4P * t1 * t2 - t4P * t7 * t2 - t4P * t7 * t1 + t5 * t1P * t4 + t5 * t1P * t3
         + t4P * t3 - t4P * t5 - t4P * t6 - t4P * t7 + t4P * t2 + t4P * t1 + t5 * t1P * t2 * t4 + t5 * t1P * t3 * t4 + t5 * t1P * t2 * t3 + t6
         * t2P * t1 * t4 + t6 * t2P * t3 * t4 + t6 * t2P * t1 * t3 + t7 * t3P * t2 * t4 + t7 * t3P * t1 * t2 + t7 * t3P * t1 * t4 - t4P * t7 * t1 * t2 - t4P
         * t6 * t2 * t3 - t4P * t6 * t1 * t2 - t4P * t7 * t2 * t3 - t4P * t7 * t1 * t3 - t4P * t6 * t1 * t3 - t4P * t5 * t1 * t2 - t4P * t5 * t1 * t3 - t4P
         * t5 * t2 * t3 + t4P * t1 * t2 * t3 - t4P * t6 * t1 * t2 * t3 + t5 * t1P * t2 * t3 * t4 + t6 * t2P * t1 * t3 * t4 + t7 * t3P * t1 * t2 * t4 - t4P
         * t7 * t1 * t2 * t3) / t1;

         if (k > 0)
            test = Math.Abs(A16 / A[1, 6]);

         A[1, 6] += A16;
         k++;

      } while (test > Criteria && k < maxIter);


      if (k == maxIter)
      {
         iterFlag = 0;
         return;
      }

      //A17
      test = 100.0;
      k = 0;
      do
      {
         double t1P = Math.Pow((t1 / (1 + t1)), k);
         double t2P = Math.Pow((t2 / (1 + t2)), k);
         double t3P = Math.Pow((t3 / (1 + t3)), k);
         double t4P = Math.Pow((t4 / (1 + t4)), k);

         double A17 = -(1 + t2) * t5 * t1P * (-k * t3P - k * t3P * t4 + t4P * k + t4P * k * t3 + t3P * t1 + t3P * t1 * t4 - t4P * t1 - t4P * t1 * t3) / (1
         + t1) / t1 / (t7 * t3P + t6 * t2P + t5 * t1P + t4P - t4P * t5 * t1 * t2 * t3 + t5 * t1P * t2 + t6 * t2P * t4 + t6 * t2P * t3 + t6 * t2P
         * t1 + t7 * t3P * t4 + t7 * t3P * t2 + t7 * t3P * t1 - t4P * t6 * t2 - t4P * t7 * t3 - t4P * t5 * t1 + t4P * t2 * t3 - t4P * t5 * t3 - t4P
         * t5 * t2 + t4P * t1 * t3 - t4P * t6 * t3 - t4P * t6 * t1 + t4P * t1 * t2 - t4P * t7 * t2 - t4P * t7 * t1 + t5 * t1P * t4 + t5 * t1P * t3
         + t4P * t3 - t4P * t5 - t4P * t6 - t4P * t7 + t4P * t2 + t4P * t1 + t5 * t1P * t2 * t4 + t5 * t1P * t3 * t4 + t5 * t1P * t2 * t3 + t6
         * t2P * t1 * t4 + t6 * t2P * t3 * t4 + t6 * t2P * t1 * t3 + t7 * t3P * t2 * t4 + t7 * t3P * t1 * t2 + t7 * t3P * t1 * t4 - t4P * t7 * t1 * t2 - t4P
         * t6 * t2 * t3 - t4P * t6 * t1 * t2 - t4P * t7 * t2 * t3 - t4P * t7 * t1 * t3 - t4P * t6 * t1 * t3 - t4P * t5 * t1 * t2 - t4P * t5 * t1 * t3 - t4P
         * t5 * t2 * t3 + t4P * t1 * t2 * t3 - t4P * t6 * t1 * t2 * t3 + t5 * t1P * t2 * t3 * t4 + t6 * t2P * t1 * t3 * t4 + t7 * t3P * t1 * t2 * t4 - t4P
         * t7 * t1 * t2 * t3);

         if (k > 0)
            test = Math.Abs(A17 / A[1, 7]);

         A[1, 7] += A17;
         k++;

      } while (test > Criteria && k < maxIter);


      if (k == maxIter)
      {
         iterFlag = 0;
         return;
      }
   }
}