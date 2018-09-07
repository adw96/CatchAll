//*********************************************************************
//**  MapleToCSharp.cs  Command Line for Windows and Linxu          ***
//*********************************************************************
using System;
using System.Configuration;
using System.Diagnostics;
using System.IO;
using System.Text.RegularExpressions;

public class MapleToCSharp
{
	//LINUX
	string slash = "/";

	//Windows
	//string slash = "\\";

	int obsMax;
    int freqMax;
	int freqTau10;
    double ACE1Tau10Rule;
    static double BigChiSq = 1000000000.0;

    static double Criteria = 0.0000000000000001;

    static int maxIter = 100000;

	//level = number of observations
	int[] freq;
	double [] observedCount;
	double[] s;
	double[] n;
	double[] lnSFactorial;
	double[] sumlnFFactorial;
	double[] sumFlnFFactorial;
    double[] w;
    double[] y;
    double[] lnW;
    double[] lnY;
    int[] WLRMSwitch;
    double[] WLRMGOF0;


    //level = models (1-7), tau
    //models 1=Poisson 2=SingleExp 3=TwoMixedExp 4=ThreeMixedExp 
    //5=FourMixedExp, 6=WLRM, 7=non-parametric;
	string[,] bubblePlotData;
	int[,]    bubblePlotDataFlag;

    //level = models (1-7), tau
    //models 1=Poisson 2=SingleExp 3=TwoMixedExp 4=ThreeMixedExp 
    //5 =FourMixedExp, 6=TWLRM, 7=UWLRM
	double[,] GOF5Test;

    //level = Criteria (0-3), model (1-7), tau
    //models 1=Poisson 2=SingleExp 3=TwoMixedExp 4=ThreeMixedExp,
    //5 =FourMixedExp, 6=TWLRM, 7=UWLRM
	double[, ,] bestGOF0;
	double[, ,] bestAICc;


	public static void Main(string[] args)
	{
		MapleToCSharp MapleData = new MapleToCSharp();
		
		//input comes from .csv file
		string inputFile = "";
		string outputPath = "";
		string dataSetName = "";
        int fourMixed = 0;

        Console.WriteLine("CatchAll Version 4.0\n");

		//  Check command line arguments
		int IOFlag = MapleData.CheckArguments(args,
             ref inputFile, ref outputPath, ref dataSetName, ref fourMixed);

		if (IOFlag == 1)
			return;

        // Frequency Range for Analysis, model dependent
        // 1=Poisson, 2=1-expl, 3=2-expl, 4=3-exp, 5=4-exp, 6=TWLRM, 7=UWLRM,
        // 8=nonparametric
        int[] fMin = { 0, 4, 4, 6, 8, 10, 5, 5 };
        int[] numParmeters = { 0, 1, 1, 3, 5, 7, 2, 2 };
        int[] fMinFlag = { 0, 0, 0, 0, 0, 0, 0, 0 };
        string[] modelDescription = {"None", "Poisson", "SingleExp", 
			"TwoMixedExp", "ThreeMixedExp", "FourMixedExp", "LogTransfWLR",
            "UnTransfWLR", "Chao1", "ACE", "ACE1"}; 

		//*********************************************************************//
		// PROGRAMS OPTIONS
	
		// Criteria for goodness of fit test
		double maxGOF = 10.0;

		//********************************************************************/
		//  read input data
		int dataFlag =  MapleData.InputDataFromFile(inputFile);

		//Make sure output directory in place
		if (!Directory.Exists(outputPath))
			Directory.CreateDirectory(outputPath);
		
		//  Open analysis outputfile
		if (dataFlag == 1)
		{
			string outputFileName = outputPath + dataSetName;
			StreamWriter swAnalysis = new StreamWriter(outputFileName +
				"_Analysis.csv");

			//write out column headers for analysis file
			swAnalysis.Write("Model,Cutoff,Observed Sp,Estimated Total Sp,");
			swAnalysis.Write("SE,Lower CB,Upper CB,Chi Sq,GOF0,GOF5,AIC,AICc,");
			swAnalysis.WriteLine("T1,T2,T3,T4,T5,T6,T7,CV Rare");


			// Get factorials, s (num species), n (num individuals)
			//for all tau--needed by all models
            MapleData.BasicStatistics(ref fMinFlag, fMin);

			int[] bestCount = new int[4];

            int modelNum = 1;
            Console.WriteLine("{0} Model", modelDescription[modelNum]);
            if (fMinFlag[modelNum] == 1)
                MapleData.PoissonModel(modelDescription[modelNum], swAnalysis,
                    fMin[modelNum], numParmeters[modelNum], modelNum, maxGOF,
                    bestCount);

            modelNum = 2;
            Console.WriteLine("{0} Model", modelDescription[modelNum]);
            if (fMinFlag[modelNum] == 1)
                MapleData.SingleExponentialModel(modelDescription[modelNum],
                    swAnalysis, fMin[modelNum], numParmeters[modelNum],
                    modelNum, maxGOF, bestCount);

            modelNum = 3;
            Console.WriteLine("{0} Model", modelDescription[modelNum]);
            if (fMinFlag[modelNum] == 1)
                MapleData.TwoMixedExponentialModel(modelDescription[modelNum],
                    swAnalysis, fMin[modelNum], numParmeters[modelNum],
                    modelNum, maxGOF, bestCount);

            modelNum = 4;
            Console.WriteLine("{0} Model", modelDescription[modelNum]);
            if (fMinFlag[modelNum] == 1)
                MapleData.ThreeMixedExponentialModel(modelDescription[modelNum],
                    swAnalysis, fMin[modelNum], numParmeters[modelNum],
                    modelNum, maxGOF, bestCount);

            if (fourMixed == 1)
            {
                modelNum = 5;
                Console.WriteLine("{0} Model", modelDescription[modelNum]);
                if (fMinFlag[modelNum] == 1)
                    MapleData.FourMixedExponentialModel(modelDescription[modelNum],
                        swAnalysis, fMin[modelNum], numParmeters[modelNum],
                    modelNum, maxGOF, bestCount);
            }

            modelNum = 6;
            Console.WriteLine("{0} Model", modelDescription[modelNum]);
            if (fMinFlag[modelNum] == 1)
                MapleData.LogTWLRModel(modelDescription[modelNum],
                    swAnalysis, fMin[modelNum], numParmeters[modelNum],
                    modelNum, maxGOF, bestCount);

            modelNum = 7;
            Console.WriteLine("{0} Model", modelDescription[modelNum]);
            if (fMinFlag[modelNum] == 1)
                MapleData.WLRModel(modelDescription[modelNum],
                    swAnalysis, fMin[modelNum], numParmeters[modelNum],
                    modelNum, maxGOF, bestCount);

            modelNum = 8;
            Console.WriteLine("Non-parametric Models");
            double cvrare = 0.0;
            if (fMinFlag[1] == 1)
                cvrare = MapleData.NonParametricModels(swAnalysis, fMin[1],
                    modelNum);

            swAnalysis.Close();

            //Write out Bubble Plot data
            MapleData.WriteBubblePlotFile(outputFileName);

            //Get Best Models
            MapleData.BestModels(bestCount, outputFileName,
               modelDescription, cvrare);
		}
		else
			Console.WriteLine("Input data file must have more than one row");

		Console.WriteLine("End of Analysis");
	}



//*********************************************************************
  //**                 Single Exponential Fits                        ***
  //*********************************************************************
  public void SingleExponentialFits(int r, ref double mlesSExp,
                                    ref int fitsCheck, ref double[] fitsCount)
{
  //find maximum likelihood estimator
  mlesSExp = (n[r] / s[r]) - 1.0;
  fitsCheck = 1;
  
  for (int t = 1; t <= freq[r]; t++)
  {
    fitsCount[t] = s[r] * (1.0 / mlesSExp) *
      Math.Pow((mlesSExp / (1.0 + mlesSExp)), t);
    
    //if fits is negative, throw flag for bad fits
    if (fitsCount[t] < 0.0)
      fitsCheck = 0;
  }
  }

//*********************************************************************
	//**                  Poisson Fits                                  ***
	//*********************************************************************
	public void PoissonFits(int r, ref double mlesPoisson, 
		ref double mlesPoissonExp, ref int fitsCheck, ref double lnFactorial, 
		ref double[] fitsCount)
	{
		double s0Init = (s[r] /
			(1.0 - (observedCount[1] / n[r]))) - s[r];

		double poissonConstant = n[r] / s[r];

		double momentsInit = n[r] / (s0Init + s[r]);

		//find maximum likelihood estimator
		int flag = 1;
		mlesPoisson = GetPoissonMles(poissonConstant, momentsInit, ref flag);
		fitsCheck = flag;

		//proceed if mles ok
		if (fitsCheck == 1)
		{
			mlesPoissonExp = Math.Exp(-mlesPoisson);

			lnFactorial = 0.0;
			for (int t = 1; t <= freq[r]; t++)
			{
				lnFactorial = lnFactorial + Math.Log(t);
				fitsCount[t] = Math.Log(s[r]) + Math.Log(mlesPoissonExp) +
					(t * Math.Log(mlesPoisson)) - Math.Log(1.0 - mlesPoissonExp) -
					 lnFactorial;

				fitsCount[t] = Math.Exp(fitsCount[t]);

				//if fits is negative, throw flag for bad fits
				if (fitsCount[t] < 0.0)
					fitsCheck = 0;
			}
		}
	}
	
	
	//*********************************************************************
  //**                          Poisson Model                         ***
  //*********************************************************************
  public void PoissonModel(string modelName, StreamWriter swAnalysis,
                           int fMin, int numParams, int modelNum, double maxGOF, 
                           int [] bestCount)
{
  string[] T = new string[7];
  
  //find fmin frequency
  int r = 1;
  while (freq[r] < fMin)
    r++;
  int freqMin = r;
  
  for (r = freqMin; r <= obsMax; r++)
  {
    double mlesPoisson = 0.0;
    double mlesPoissonExp = 0.0;
    int fitsCheck = 1;
    double lnFactorial = 0.0;
    
    double[] fitsCount = new double[freq[r] + 1];
    
    PoissonFits(r, ref mlesPoisson, ref mlesPoissonExp, 
                ref fitsCheck, ref lnFactorial, 
                ref fitsCount);
    
    //proceed if no negative fits calculated
    if (fitsCheck == 1)
    {
      //compute fitted values 
      int extendedTau = freq[r] * 4;
      double[] fitsExtended = new double[extendedTau + 1];
      
      for (int t = 1; t <= freq[r]; t++)
        fitsExtended[t] = fitsCount[t];
      
      //compute extended fitted values 
      for (int t = (freq[r] + 1); t <= extendedTau; t++)
      {
        lnFactorial = lnFactorial + Math.Log(t);
        
        fitsExtended[t] = Math.Log(s[r]) + Math.Log(mlesPoissonExp) +
          (t * Math.Log(mlesPoisson)) - Math.Log(1.0 - mlesPoissonExp) -
          lnFactorial;
        
        fitsExtended[t] = Math.Exp(fitsExtended[t]);
      }
      
      //estimated total species			
      double sHatSubset = (s[r] * mlesPoissonExp) /
        (1.0 - mlesPoissonExp) + s[r];
      
      double sHatTotal = sHatSubset + (s[obsMax] - s[r]);
      
      double part1 = lnSFactorial[r] - sumlnFFactorial[r];
      
      double part2 = (-s[r] * mlesPoisson) + (n[r] *
                                                Math.Log(mlesPoisson)) - sumFlnFFactorial[r] -
        (s[r] * Math.Log(1.0 - mlesPoissonExp));
      
      double AIC = 0.0;
      double AICc = 0.0;
      double chiSqAll = 0.0;
      double GOF0 = 0.0;
      double GOF5 = 0.0;
      
      int AICcCheck = 0;
      int GOF0Check = 0;
      int GOF5Check = 0;
      
      GOF5Test[modelNum, r] = CalculateAnalysisVariables(part1,
                                                         part2, numParams, r, fitsCount, fitsExtended, maxGOF,
                                                         ref AIC, ref AICc, ref AICcCheck, ref chiSqAll, ref GOF0,
                                                         ref GOF5, ref GOF0Check, ref GOF5Check, modelNum);
      
      //Standard Error
      double SE = sHatSubset / (Math.Exp(mlesPoisson) -
                                  1.0 - mlesPoisson);
      int SEFlag = 0;
      
      if (SE > 0.0)
      {
        SE = Math.Sqrt(SE);
        SEFlag = 1;
      }
      
      //Confidence Bounds
      double LCB = 0.0;
      double UCB = 0.0;
      int boundsCheck = 0;
      
      if (SEFlag == 1)
        boundsCheck = GetConfidenceBounds(r, SE, sHatSubset, 
                                          ref LCB, ref UCB);
      
      //Write out Analysis row for this dataset/model/tau
      T[0] = mlesPoisson.ToString();
      WriteAnalysisRow(swAnalysis, modelName, r, sHatTotal,
                       SEFlag, SE, boundsCheck, LCB, UCB, GOF0Check, GOF0,
                       GOF5Check, GOF5, AICcCheck, AICc, T, chiSqAll, AIC,
                       modelNum, bestCount);		
    }
    
    else
      //Write out Analysis row as all missing
    swAnalysis.WriteLine("{0},{1},{2},,,,,,,,,,,,,,,,,,",
                         modelName, freq[r], s[r]);
  }
  }
  
//*********************************************************************
  //**                 Single Exponential Model                       ***
  //*********************************************************************
  public void SingleExponentialModel(string modelName,
                                     StreamWriter swAnalysis, int fMin, int numParams, int modelNum, 
                                     double maxGOF, int[] bestCount)
{
  string[] T = new string[7];
  
  //find fmin frequency
  int r = 1;
  while (freq[r] < fMin)
    r++;
  int freqMin = r;
  
  for (r = freqMin; r <= obsMax; r++)
  {
    double mlesSExp = 0.0;
    int fitsCheck = 1;
    
    double[] fitsCount = new double[freq[r] + 1];
    
    SingleExponentialFits(r, ref mlesSExp, ref fitsCheck, 
                          ref fitsCount);
    
    
    // proceed if no negative fits calculated
    if (fitsCheck == 1)
    {
      //compute fitted values 
      int extendedTau = freq[r] * 4;
      double[] fitsExtended = new double[extendedTau + 1];
      
      for (int t = 1; t <= freq[r]; t++)
        fitsExtended[t] = fitsCount[t];
      
      //compute extended fitted values 
      for (int t = (freq[r] + 1); t <= extendedTau; t++)
        fitsExtended[t] = s[r] * (1.0 / mlesSExp) *
        Math.Pow((mlesSExp / (1.0 + mlesSExp)), t);
      
      //estimated total species				
      double sHatSubset = (n[r] * s[r]) / (n[r] - s[r]);
      
      double sHatTotal = sHatSubset + (s[obsMax] - s[r]);
      
      double part1 = lnSFactorial[r] - sumlnFFactorial[r];
      
      double part2 = (n[r] - s[r]) * Math.Log(mlesSExp) -
        n[r] * Math.Log(1.0 + mlesSExp);
      
      double AIC = 0.0;
      double AICc = 0.0;
      double chiSqAll = 0.0;
      double GOF0 = 0.0;
      double GOF5 = 0.0;
      
      int AICcCheck = 0;
      int GOF0Check = 0;
      int GOF5Check = 0;
      
      GOF5Test[modelNum, r] = CalculateAnalysisVariables(part1, part2, 
                                                         numParams, r,fitsCount, fitsExtended, maxGOF, ref AIC,
                                                         ref AICc, ref AICcCheck, ref chiSqAll, ref GOF0, ref GOF5,
                                                         ref GOF0Check, ref GOF5Check, modelNum);
      
      //Standard Error
      double SE = sHatSubset / (Math.Sqrt(n[r] - s[r]));
      int SEFlag = 1;
      
      //Confidence Bounds
      double LCB = 0.0;
      double UCB = 0.0;
      
      int boundsCheck = GetConfidenceBounds(r, SE, sHatSubset,
                                            ref LCB, ref UCB);
      
      //Write out Analysis row for this dataset/model/tau
      T[0] = mlesSExp.ToString();
      WriteAnalysisRow(swAnalysis, modelName, r, sHatTotal,
                       SEFlag, SE, boundsCheck, LCB, UCB, GOF0Check, GOF0,
                       GOF5Check, GOF5, AICcCheck, AICc, T, chiSqAll, AIC,
                       modelNum, bestCount);
      
    }
    else
      //Write out Analysis row as all missing
    swAnalysis.WriteLine("{0},{1},{2},,,,,,,,,,,,,,,,,,",
                         modelName, freq[r], s[r]);
  }
  }
  
	//*********************************************************************
	//**                          ACE Model                             ***
	//*********************************************************************
	public void ACEModel(StreamWriter swAnalysis, int freqMin,
		double[] GTEstimate, double[] G, double[] gammaSqRare,
		double[] cvRare, int modelNum, double singletons)
	{
		//f1
		double f1 = singletons;

		for (int r = freqMin; r <= obsMax; r++)
		{			
			//estimated total species	
			double sHatSubset = GTEstimate[r] + ((f1 / (1.0 - (f1 /
				n[r])) * gammaSqRare[r]));

			double sHatTotal = sHatSubset + (s[obsMax] - s[r]);

			//Standard Error
			double derCHat1 = ((n[r] + s[r]) / (n[r] - f1)) +
								(((f1 + n[r]) / (n[r] - f1)) *
								((s[r] / (n[r] - f1)) * (G[r] / (n[r] - 1.0)) - 1.0)) +
								((n[r] * f1 * G[r] * (n[r] - 1.0 - s[r])) /
								(((n[r] - f1) * (n[r] - f1) * (n[r] - 1.0) *
								(n[r] - 1.0))));

			double A = derCHat1 * derCHat1 * f1 * (1.0 - (f1 / sHatSubset));

			double[] derCHati = new double[freq[r] + 1];

			for (int i = 2; i <= freq[r]; i++)
			{
				derCHati[i] = (((n[r] * n[r]) - (f1 * n[r]) -
					(f1 * s[r] * i)) / ((n[r] - f1) * (n[r] - f1))) -
					(((f1 * f1 * i) / ((n[r] - f1) * (n[r] - f1))) *
					((s[r] / (n[r] - f1)) * (G[r] / (n[r] - 1.0)) - 1.0)) +
					(((n[r] * f1) / (n[r] - f1)) *
					(((n[r] - f1 - s[r] * i) / ((n[r] - f1) *
					(n[r] - f1))) * (G[r] / (n[r] - 1.0)) +
					(((s[r] / (n[r] - f1)) * (((n[r] - 1.0) * i *
					(i - 1.0)) - G[r] * i) / ((n[r] - 1.0) *
					(n[r] - 1.0))))));
			}

			double B = 0.0;
			double C = 0.0;
			double D = 0.0;

			int rr = 1;
			while (freq[rr] < 2)
			{
				rr++;
			} 

			for (int i = 2; i <= freq[r]; i++)
			{
				if (i == freq[rr])
				{
					B += (derCHati[i] * derCHati[i] * observedCount[rr] *
						(1.0 - (observedCount[rr] / sHatSubset)));

					C += (derCHat1 * derCHati[i] * (-f1 * observedCount[rr] /
						sHatSubset));

					int rj = 1;
					while (freq[rj] < i + 1 && rj < r)
					{
						rj++;
					}

					for (int j = i + 1; j <= freq[r]; j++)
					{
						if (j == freq[rj])
						{
							D += (derCHati[i] * derCHati[j] * (-observedCount[rr] *
								observedCount[rj] / sHatSubset));
							rj++;
						}							
					}
					rr++;
				}					
			}

			double SE = A + B + 2.0 * C + 2.0 * D;
			int SEFlag = 0;
			if (SE > 0.0)
			{
				SE = Math.Sqrt(SE);
				SEFlag = 1;
			}

			if (SEFlag == 1)
			{
				//Confidence Bounds
				double LCB = 0.0;
				double UCB = 0.0;
				GetConfidenceBounds(r, SE, sHatSubset, ref LCB, ref UCB);

				//Write out Analysis row for this dataset/model/tau
				swAnalysis.Write("ACE,{0},{1},{2},{3},{4},{5}",
					freq[r], s[r], sHatTotal, SE, LCB, UCB);
				swAnalysis.WriteLine(",,,,,,,,,,,,,{0}",
					cvRare[r]);
			}
			// Bad SE
			else
				swAnalysis.WriteLine("ACE,{0},{1},{2},,,,,,,,,,,,,,,,{3}",
					freq[r], s[r], sHatTotal, cvRare[r]);

			//Bubble Plot file
			if (cvRare[r] <= 0.8)
			{
				bubblePlotDataFlag[modelNum - 1, r] = SEFlag;
				bubblePlotData[modelNum - 1, r] = sHatTotal.ToString() + "," +
					SE.ToString();
			}
		}
	}

	//*********************************************************************
	//**                         ACE1 Model                             ***
	//*********************************************************************
	public void ACE1Model(StreamWriter swAnalysis, int freqMin,
		double[] GTEstimate, double[] gammaSqRare, double[] cvRare,
		double[] G, int modelNum, double singletons)
	{
		//f1
		double f1 = singletons;

		for (int r = freqMin; r <= obsMax; r++)
		{
			double gammaSqRarePrime = 0.0;
			if (gammaSqRare[r] > 0.0)
			{
				gammaSqRarePrime = gammaSqRare[r] * (1.0 +
					((f1 / n[r]) / (1.0 - (f1 / n[r])) *
					(G[r] / (n[r] - 1.0))));
			}

			//estimated total species	
			double sHatSubset = GTEstimate[r] +
				((f1 / (1.0 - (f1 / n[r])) * gammaSqRarePrime));

			double sHatTotal = sHatSubset + (s[obsMax] - s[r]);

			//Standard Error
			double derCHat1 = ((n[r] + s[r]) / (n[r] - f1)) +
								(((f1 + n[r]) / (n[r] - f1)) *
								((s[r] / (n[r] - f1)) * (G[r] / (n[r] - 1.0)) - 1.0)) +
								((n[r] * f1 * G[r] * (n[r] - 1.0 - s[r])) /
								(((n[r] - f1) * (n[r] - f1) * (n[r] - 1.0) *
								(n[r] - 1.0))));

			//more terms for derivative
			double g = ((s[r] / (1.0 - f1 / n[r])) *
				(G[r] / (n[r] * (n[r] - 1.0)))) - 1.0;

			double h = (f1 / n[r]) / (1.0 - (f1 / n[r])) *
				(G[r] / (n[r] - 1.0));

			double der1 = (n[r] + f1) / (n[r] - f1);

			double derg = (G[r] / ((n[r] - f1) * (n[r] - 1.0))) *
				(1.0 - (s[r] / (n[r] - 1.0)));

			double derh = (G[r] / ((n[r] - f1) * (n[r] - 1.0))) *
				(1.0 - (f1 / (n[r] - 1.0)));

			derCHat1 += ((der1 * g * h) + (derg * (f1 / (1.0 - (f1 / n[r]))) * h) +
				(derh * (f1 / (1.0 - (f1 / n[r]))) * g));

			double A = derCHat1 * derCHat1 * f1 * (1.0 - (f1 / sHatSubset));

			double[] derCHati = new double[freq[r] + 1];

			for (int i = 2; i <= freq[r]; i++)
			{
				derCHati[i] = (((n[r] * n[r]) - (f1 * n[r]) -
					(f1 * s[r] * i)) / ((n[r] - f1) * (n[r] - f1))) -
					(((f1 * f1 * i) / ((n[r] - f1) * (n[r] - f1))) *
					((s[r] / (n[r] - f1)) * (G[r] / (n[r] - 1.0)) - 1.0)) +
					(((n[r] * f1) / (n[r] - f1)) *
					(((n[r] - f1 - s[r] * i) / ((n[r] - f1) *
					(n[r] - f1))) * (G[r] / (n[r] - 1.0)) +
					(((s[r] / (n[r] - f1)) * (((n[r] - 1.0) * i *
					(i - 1.0)) - G[r] * i) / ((n[r] - 1.0) *
					(n[r] - 1.0))))));

				double deri = -(f1 * f1 * i) / ((n[r] - f1) * (n[r] - f1));

				double dergi = (((n[r] - f1 - i * s[r]) /
					((n[r] - f1) * (n[r] - f1))) *
					(G[r] / (n[r] - 1.0))) + ((s[r] / (n[r] - f1)) *
					((((n[r] - 1.0) * i * (i - 1)) - (i * G[r])) /
					((n[r] - 1) * (n[r] - 1))));

				double derhi = -((f1 * i * G[r]) / ((n[r] - f1) *
					(n[r] - f1) * (n[r] - 1.0))) + ((f1 / (n[r] - f1)) *
					((((n[r] - 1.0) * i * (i - 1)) - (i * G[r])) /
					((n[r] - 1.0) * (n[r] - 1.0))));

				derCHati[i] += ((deri * g * h) + (dergi * (f1 / (1.0 - (f1 / n[r]))) * h) +
				(derhi * (f1 / (1.0 - (f1 / n[r]))) * g));
			}

			double B = 0.0;
			double C = 0.0;
			double D = 0.0;

			int rr = 1;
			while (freq[rr] < 2)
			{
				rr++;
			} 

			for (int i = 2; i <= freq[r]; i++)
			{
				if (i == freq[rr])
				{
					B += (derCHati[i] * derCHati[i] * observedCount[rr] *
						(1.0 - (observedCount[rr] / sHatSubset)));

					C += (derCHat1 * derCHati[i] * (-f1 * observedCount[rr] /
						sHatSubset));
					
					int rj = 1;
					while (freq[rj] < i + 1 && rj < r)
					{
						rj++;
					}

					for (int j = i + 1; j <= freq[r]; j++)
					{
						if (j == freq[rj])
						{
							D += (derCHati[i] * derCHati[j] * (-observedCount[rr] *
								observedCount[rj] / sHatSubset));
							rj++;
						}
					}
					rr++;
				}
			}

			double SE = A + B + 2.0 * C + 2.0 * D;
			int SEFlag = 0;
			if (SE > 0.0)
			{
				SE = Math.Sqrt(SE);
				SEFlag = 1;
			}

			if (SEFlag == 1)
			{
				//Confidence Bounds
				double LCB = 0.0;
				double UCB = 0.0;
				GetConfidenceBounds(r, SE, sHatSubset, ref LCB, ref UCB);

				//Write out Analysis row for this dataset/model/tau
				swAnalysis.Write("ACE1,{0},{1},{2},{3},{4},{5}",
					freq[r], s[r], sHatTotal, SE, LCB, UCB);
				swAnalysis.WriteLine(",,,,,,,,,,,,,{0}",
					cvRare[r]);
			}
			// Bad SE
			else
				swAnalysis.WriteLine("ACE1,{0},{1},{2},,,,,,,,,,,,,,,,{3}",
					freq[r], s[r], sHatTotal, cvRare[r]);

			//Bubble Plot file
			if (cvRare[r] > 0.8)
			{
				bubblePlotDataFlag[modelNum - 1, r] = SEFlag;
				bubblePlotData[modelNum - 1, r] = sHatTotal.ToString() + "," +
					SE.ToString();
			}
		}
	}

    //*********************************************************************
    //**                         ACE1Tau10                              ***
    //*********************************************************************
    public double ACE1Tau10()
    {
        //f1
        double f1 = 0.0;
        if (freq[1] == 1)
            f1 = observedCount[1];

        int r = freqTau10;

        //Good Turing				
        double GTEstimate = s[r] / (1.0 - (f1 / n[r]));

        double G = 0.0;
        int rr = 1;
        for (int i = 1; i <= freq[r]; i++)
        {
            if (i == freq[rr])
            {
                G += (i * (i - 1.0) * observedCount[rr]);
                rr++;
            }
        }

        double gammaSqRare = (GTEstimate * G /
            (n[r] * (n[r] - 1.0))) - 1.0;

        if (gammaSqRare < 0.0)
            gammaSqRare = 0.0;

        double gammaSqRarePrime = 0.0;
        if (gammaSqRare > 0.0)
        {
            gammaSqRarePrime = gammaSqRare * (1.0 +
                ((f1 / n[r]) / (1.0 - (f1 / n[r])) *
                (G / (n[r] - 1.0))));
        }

        //estimated total species	
        double sHatSubset = GTEstimate +
            ((f1 / (1.0 - (f1 / n[r])) * gammaSqRarePrime));

        double sHatTotal = sHatSubset + (s[obsMax] - s[r]);

        return sHatTotal * 100;
    }
    
    //*********************************************************************
	//***                    Basic Statistics                           ***
	//*********************************************************************
    public void BasicStatistics(ref int[] fMinFlag, int[] fMin)
	{
        //find max frequency; all frequencies have to be contiguous
        int a = 1;
        while (a <= obsMax && freq[a] == a)
            a++;

        //freqMax used instead of obsMax for WRLM models
        freqMax = a - 1;

        //is data adequate to calculate the given model
        for (int m = 1; m < 6; m++)
            if (obsMax >= fMin[m])
                fMinFlag[m] = 1;

        for (int m = 6; m < 8; m++)
            if (freqMax >= fMin[m])
                fMinFlag[m] = 1;

		//s = number of species
		s = new double[obsMax + 1];

		//n = number of individuals
		n = new double[obsMax + 1];

		//ln of s!
		lnSFactorial = new double[obsMax + 1];

		//sum of ln of observerdCount (f)
		sumlnFFactorial = new double[obsMax + 1];

		sumFlnFFactorial = new double[obsMax + 1];

		for (int r = 1; r <= obsMax; r++)
		{
			s[r] = 0;
			n[r] = 0.0;
			lnSFactorial[r] = 0.0;
			sumlnFFactorial[r] = 0.0;

			for (int t = 1; t <= r; t++)
			{
				s[r] += observedCount[t];
				n[r] += (freq[t] * observedCount[t]);
			}
				
			//log of s!
			for (int i = 1; i <= s[r]; i++)
				lnSFactorial[r] += Math.Log(i);

			//log of f! where f = observedCount
			for (int t = 1; t <= r; t++)
			{
				int f = (int) observedCount[t];	
			
				double lnFFactorial = 0.0;
				for (int i = 1; i <= f; i++)
					lnFFactorial += Math.Log(i);

				sumlnFFactorial[r] += lnFFactorial;

				double lnIFactorial = 0.0;
				for (int i = 1; i <= freq[t]; i++)
					lnIFactorial += Math.Log(i);

				lnIFactorial *= f;

				sumFlnFFactorial[r] += lnIFactorial;
			}
        }

        //data for weighted linear regression
        y = new double[freqMax];
        w = new double[freqMax];

        lnY = new double[freqMax];
        lnW = new double[freqMax];

        WLRMSwitch = new int[freqMax + 1];
        WLRMGOF0 = new double[freqMax + 1];

        //calculate ratios
        for (int i = 1; i < freqMax; i++)
        {
            y[i] = (i + 1.0) * observedCount[i + 1] / observedCount[i];
            lnY[i] = Math.Log(y[i]);

            w[i] = (observedCount[i] * observedCount[i] * observedCount[i]) /
                ((i + 1.0) * (i + 1.0) * observedCount[i + 1] *
                (observedCount[i] + observedCount[i + 1]));

            lnW[i] = (observedCount[i] * observedCount[i + 1]) /
                (observedCount[i] + observedCount[i + 1]);

            //start with switch to logged version
            WLRMSwitch[i] = 0;
        }

        //calculate ACE1 at tau = 10 for best model comparisons
        ACE1Tau10Rule = ACE1Tau10();
	}

	//*********************************************************************
	//**                 Find 4 Best Model/Tau combinations             ***
	//*********************************************************************
	public void BestModels(int[] bestCount, string outputFileName,
		string[] modelDescription, double cvrare)
	{
		int[] bestModel = new int[obsMax + 1];

		int[,] bestModelTau = new int[10, 2];

		//Use strictest Criteria possible (most to least strict 1,2,0, 3)
		int flag = -1;
		if (bestCount[1] > 0)
			flag = 1;
		else if (bestCount[2] > 0)
			flag = 2;
		else if (bestCount[0] > 0)
			flag = 0;

		//Apply entirely different Criteria if all filters fail
		if (flag == -1 && bestCount[3] > 0)
		{
			flag = 3;
			double[] testValue = new double[obsMax + 1];
			int [] order = new int[obsMax + 1];
			for (int r = 1; r <= obsMax; r++)
			{
				double minAICc = 10000000;

				for (int m = 1; m < 6; m++)
				{
					if (bestAICc[flag, m, r] > 0.0 &&
						bestAICc[flag, m, r] < minAICc)
					{
						bestModel[r] = m;
						minAICc = bestAICc[flag, m, r];
					}
				}
				testValue[r] = GOF5Test[bestModel[r], r];	
				order[r] = r;
			}
			Array.Sort(testValue, order);

			int bm = 0;
			int b = 0;
			do
			{
				if (testValue[b] > 0)
				{
					bestModelTau[bm, 0] = bestModel[order[b]];
					bestModelTau[bm, 1] = order[b];
					bm++;
				}
				b++;
            } while (bm < 4 && b <= obsMax);
		}
			
		//If there are candidate models proceed to find best ones
		else if (flag >= 0)
		{
			//For each tau, find the model with the minimum AICc 
			for (int r = 1; r <= obsMax; r++)
			{
				double minAICc = 10000000;

				for (int m = 1; m < 6; m++)
				{
					if (bestAICc[flag, m, r] > 0.0 &&
						bestAICc[flag, m, r] < minAICc)
					{
						bestModel[r] = m;
						minAICc = bestAICc[flag, m, r];
					}
				}
			}

			double maxGOF0 = bestGOF0[flag, bestModel[freqTau10], freqTau10];
			for (int r = 1; r <= obsMax; r++)
			{
				if (bestModel[r] > 0)
				{
					//Find Best Model--largest tau with GOF0 >= 0.01
					if (bestGOF0[flag, bestModel[r], r] >= 0.01)
					{
						bestModelTau[0, 0] = bestModel[r];
						bestModelTau[0, 1] = r;
					}

					//Find Good Model 1--tau with largest GOF0
					if (bestGOF0[flag, bestModel[r], r] >= maxGOF0)
					{
						bestModelTau[1, 0] = bestModel[r];
						bestModelTau[1, 1] = r;
						maxGOF0 = bestGOF0[flag, bestModel[r], r];
					}

					//Find Good Model 2--largest tau with data
					if (bestGOF0[flag, bestModel[r], r] > 0)
					{
						bestModelTau[2, 0] = bestModel[r];
						bestModelTau[2, 1] = r;
					}
				}
			}

			//Find Good Model 3--tau closest to 10 with data
			if (bestGOF0[flag, bestModel[freqTau10], freqTau10] > 0)
			{
				bestModelTau[3, 0] = bestModel[freqTau10];
				bestModelTau[3, 1] = freqTau10;
			}
			else
			{
				//look > tau10
				int t = freqTau10 + 1;
				while (bestModelTau[3, 1] == 0 && t <= obsMax)
				{
					if (bestModel[t] > 0)
					{

						if (bestGOF0[flag, bestModel[t], t] > 0)
						{
							bestModelTau[3, 0] = bestModel[t];
							bestModelTau[3, 1] = t;
						}
					}
					t++;
				}

				//look < tau10
				t = freqTau10 - 1;
				while (bestModelTau[3, 1] == 0 && t > 0)
				{
					if (bestModel[t] > 0)
					{

						if (bestGOF0[flag, bestModel[t], t] > 0)
						{
							bestModelTau[3, 0] = bestModel[t];
							bestModelTau[3, 1] = t;
						}
					}
					t--;
				}
			}
		}

        //Find Good Model 4--best WLRM
        //if there are taus with G0FO >= 0.01, find the max tau
      if (freqMax > 0)
      {
         bestModelTau[4, 0] = 6;
         double maxGOF0WLRM = 0.01;
         bestModelTau[4, 1] = 0;
         for (int r = 1; r <= freqMax; r++)
         {
            //determine whether to use logged (6) or unlogged(7) version
            if (WLRMSwitch[r] == 0)
            {
               if (bestGOF0[0, 6, r] >= maxGOF0WLRM)
               {
                  bestModelTau[4, 0] = 6;
                  bestModelTau[4, 1] = r;
               }
            }
            else if (WLRMSwitch[r] == 1)
            {
               if (bestGOF0[0, 7, r] >= maxGOF0WLRM)
               {
                  bestModelTau[4, 0] = 7;
                  bestModelTau[4, 1] = r;
               }
            }
         }

         //if there are no taus with GOF0 >= 0.01
         //use tau with highest GOF0
         if (bestModelTau[4, 1] == 0)
         {
            if (WLRMSwitch[1] == 0)
               maxGOF0WLRM = bestGOF0[0, 6, 1];
            else if (WLRMSwitch[1] == 1)
               maxGOF0WLRM = bestGOF0[0, 7, 1];

            for (int r = 2; r <= freqMax; r++)
            {
               if (WLRMSwitch[r] == 0)
               {
                  if (bestGOF0[0, 6, r] >= maxGOF0WLRM)
                  {
                     bestModelTau[4, 0] = 6;
                     bestModelTau[4, 1] = r;
                     maxGOF0WLRM = bestGOF0[0, 6, r];
                  }
               }
               else if (WLRMSwitch[r] == 1)
               {
                  if (bestGOF0[0, 7, r] >= maxGOF0WLRM)
                  {
                     bestModelTau[4, 0] = 7;
                     bestModelTau[4, 1] = r;
                     maxGOF0WLRM = bestGOF0[0, 7, r];
                  }
               }
            }
         }
      }

      //if there are none, leave blank
      if (bestModelTau[4, 1] == 0)
         bestModelTau[4, 0] = 0;

		//Find Chao1 Model--all taus give the  same answer
		bestModelTau[5, 0] = 8;
        bestModelTau[5, 1] = freqTau10;

		//Find ACE/ACE1 Model closest to tau = 10
		if (cvrare <= 0.8)
			bestModelTau[6, 0] = 9;
		else
			bestModelTau[6, 0] = 10;
		bestModelTau[6, 1] = freqTau10;

		//Max Tau for Best Model
		if (flag >= 0)
		{
			bestModelTau[7, 0] = bestModelTau[0, 0];
			bestModelTau[7, 1] = obsMax;
		}

        //Max Tau for WLRM Model
        if (WLRMSwitch[freqMax] == 0)
            bestModelTau[8, 0] = 6;
        else if (WLRMSwitch[freqMax] == 1)
            bestModelTau[8, 0] = 7;

        //if there are none 
        if (bestModelTau[4, 0] == 0)
            bestModelTau[8, 0] = 0;
        bestModelTau[8, 1] = freqMax;

      //ACE Model at tau = 10 or < 10 if necessary
	   bestModelTau[9, 0] = 9;
      bestModelTau[9, 1] = freqTau10;

		if (flag >= 0)
		    WriteBestModelsFitsFile(outputFileName, bestModelTau,
			    modelDescription);

		WriteBestModelsAnalysisFile(outputFileName, bestModelTau,
			modelDescription, cvrare);
	}

	//*********************************************************************
	//***              Find Bracket Roots (+ and -)                     ***
	//*********************************************************************
	public static int BracetRoot(double poissonConstant, double momentsInit,
			ref double x1, ref double x2, ref double f1, ref double f2)
	{
		//  initial guess range
		double factor = 2.0;
		x1 = momentsInit / factor;
		x2 = momentsInit * factor;

		f1 = (x1 / (1.0 - Math.Exp(-x1))) - poissonConstant;
		f2 = (x2 / (1.0 - Math.Exp(-x2))) - poissonConstant;

		for (int i = 0; i < 20; i++)
		{
			// one estimate is negative and the other positive
			if ((f1 * f2) < 0.0)
				return 1;

			// move the appropriate bound
			if (Math.Abs(f1) < Math.Abs(f2))
			{
				x1 += factor * (x1- x2);
				f1 = (x1 / (1.0 - Math.Exp(-x1))) - poissonConstant;
			}
			else
			{
				x2 += factor * (x2 - x1);
				f2 = (x2 / (1.0 - Math.Exp(-x2))) - poissonConstant;
			}
		}

		//failure
		return 0;
	}

	//*********************************************************************
	//***              Calculate Analysis Variables                     ***
	//*********************************************************************
	public double CalculateAnalysisVariables(double part1, double part2,
		int numParams, int r, double[] fitsCount, double[] fitsExtended,
		double maxGOF, ref double AIC, ref double AICc, ref int AICcFlag,
		ref double chiSqAll, ref double GOF0, ref double GOF5,
		ref int GOF0Check, ref int GOF5Check, int modelNum)
	{
		AIC = 2.0 * numParams - 2.0 * (part1 + part2);

		AICc = 0.0;
		if ((s[r] - numParams - 1) > 0.0)
		{
			AICc = AIC + (2.0 * numParams * (numParams + 1.0) /
			(s[r] - numParams - 1.0));
			AICcFlag = 1;
		}

		//calculate ChiSq, no binning
		chiSqAll = ChiSq(r, fitsCount, modelNum);

		//calculate Goodness of Fit
		int df = freq[r] - numParams;
		int flag = 1;
		GOF0 = 0.0;
		double test = (chiSqAll - df) / Math.Sqrt(2.0 * df);
		if (test < maxGOF && chiSqAll < BigChiSq)
			GOF0 = GoodNessOfFit(chiSqAll, df, ref flag);
		else
			flag = 0;
		GOF0Check = flag;

		//calculate ChiSq, bin 5
		flag = 1;
		double chiSq5 = ChiSqBin(r, fitsExtended, 5, ref df,
				numParams, ref flag);
		GOF5Check = flag;

		//calculate Goodness of Fit
		GOF5 = 0.0;
		test = (chiSq5 - df) / Math.Sqrt(2.0 * df);
		if (test < maxGOF && flag == 1 && chiSq5 < BigChiSq)
		{
			GOF5 = GoodNessOfFit(chiSq5, df, ref flag);
			GOF5Check = flag;
		}
		return test;
	}
	
	//*********************************************************************
	//**                         Chao1 Model                            ***
	//*********************************************************************
	public void Chao1Model(StreamWriter swAnalysis, int freqMin, 
		double singletons)
	{
		//f1
		double f1 = singletons;

		//estimated total species	
		double sHatTotal = 0.0;

		//Standard Error
		double SE = 0.0;
		int SEFlag = 0;

		//if there are doubletons
		if (freq[1] == 2 || freq[2] == 2)
		{
			sHatTotal = s[obsMax] + (f1 * f1 / (2.0 * observedCount[2]));

			double temp = f1 / observedCount[2];

			SE = observedCount[2] * ((0.5 * (temp * temp)) +
				(temp * temp * temp) + (0.25 * (temp * temp * temp * temp)));

			if (SE >= 0.0)
			{
				SE = Math.Sqrt(SE);
				SEFlag = 1;
			}
		}
		else
		{
			sHatTotal = s[obsMax] + (f1 * (f1 - 1.0) / 2.0);

			SE = (0.5 * f1 * (f1 - 1.0)) + (0.25 * f1 * 2.0 *
				(f1 - 1.0) * 2.0 * (f1 - 1.0)) - (0.25 * f1 * f1 * f1 * f1 /
				sHatTotal);

			if (SE >= 0.0)
			{
				SE = Math.Sqrt(SE);
				SEFlag = 1;
			}
		}

		for (int r = freqMin; r <= obsMax; r++)
		{
			double sHatSubset = sHatTotal - (s[obsMax] - s[r]);

			if (SEFlag == 1)
			{
				//Confidence Bounds
				double LCB = 0.0;
				double UCB = 0.0;
				GetConfidenceBounds(r, SE, sHatSubset, ref LCB, ref UCB);

				//Write out Analysis row for this dataset/model/tau
				swAnalysis.WriteLine("Chao1,{0},{1},{2},{3},{4},{5},,,,,,,,,,,,,",
					freq[r], s[r], sHatTotal, SE, LCB, UCB);
			}
			// Bad SE
			else
				swAnalysis.WriteLine("Chao1,{0},{1},{2},,,,,,,,,,,,,,,,",
					freq[r], s[r], sHatTotal);
		}
	}

	//*********************************************************************
	//**                      Chao Bunge Model                          ***
	//*********************************************************************
	public void ChaoBungeModel(StreamWriter swAnalysis, int freqMin, 
		double singletons)
	{
		//f1
		double f1 = singletons;

		for (int r = freqMin; r <= obsMax; r++)
		{
			double sumSqFreq = 0.0;
			for (int i = 1; i <= r; i++)
				sumSqFreq += (freq[i] * freq[i] * observedCount[i]);

			//estimated total species	
			double sHatSubset = (s[r] - f1) / (1.0 - (f1 * sumSqFreq /
				(n[r] * n[r])));

			if (sHatSubset > 0.0)
			{
				double sHatTotal = sHatSubset + (s[obsMax] - s[r]);

				//Standard Error
				double derCHat1 = (n[r] * (s[r] - f1) * ((f1 * n[r]) -
					(2.0 * f1 * sumSqFreq) + (n[r] * sumSqFreq))) /
					(((n[r] * n[r]) - (f1 * sumSqFreq)) *
					 ((n[r] * n[r]) - (f1 * sumSqFreq)));

				double A = derCHat1 * derCHat1 * f1 * (1.0 - (f1 / sHatSubset));

				double[] derCHati = new double[freq[r] + 1];

				for (int i = 2; i <= freq[r]; i++)
				{
					derCHati[i] = (n[r] * ((n[r] * n[r] * n[r]) +
						(f1 * n[r] * i * i * s[r]) - (n[r] * f1 * sumSqFreq) -
						(f1 * f1 * n[r] * i * i) - (2.0 * f1 * sumSqFreq * i * s[r]) +
						(2.0 * f1 * f1 * sumSqFreq * i))) /
						(((n[r] * n[r]) - (f1 * sumSqFreq)) *
						 ((n[r] * n[r]) - (f1 * sumSqFreq)));
				}

				double B = 0.0;
				double C = 0.0;
				double D = 0.0;

				int rr = 1;
				while (freq[rr] < 2)
				{
					rr++;
				} 

				for (int i = 2; i <= freq[r]; i++)
				{
					if (i == freq[rr])
					{
						B += (derCHati[i] * derCHati[i] * observedCount[rr] *
							(1.0 - (observedCount[rr] / sHatSubset)));

						C += (derCHat1 * derCHati[i] * (-f1 * observedCount[rr] /
							sHatSubset));

						int rj = 1;
						while (freq[rj] < i + 1 && rj < r)
						{
							rj++;
						}

						for (int j = i + 1; j <= freq[r]; j++)
						{
							if (j == freq[rj])
							{
								D += (derCHati[i] * derCHati[j] * (-observedCount[rr] *
									observedCount[rj] / sHatSubset));
								rj++;
							}
						}
						rr++;
					}						
				}

				double SE = A + B + 2.0 * C + 2.0 * D;
				int SEFlag = 0;
				if (SE > 0.0)
				{
					SE = Math.Sqrt(SE);
					SEFlag = 1;
				}

				if (SEFlag == 1)
				{
					//Confidence Bounds
					double LCB = 0.0;
					double UCB = 0.0;
					GetConfidenceBounds(r, SE, sHatSubset, ref LCB, ref UCB);

					//Write out Analysis row for this dataset/model/tau
					swAnalysis.WriteLine("ChaoBunge,{0},{1},{2},{3},{4},{5},,,,,,,,,,,,,",
						freq[r], s[r], sHatTotal, SE, LCB, UCB);
				}
				// Bad SE
				else
					swAnalysis.WriteLine("ChaoBunge,{0},{1},{2},,,,,,,,,,,,,,,,",
						freq[r], s[r], sHatTotal);
			}
			// Negative shatsubset
			else
				swAnalysis.WriteLine("ChaoBunge,{0},{1},,,,,,,,,,,,,,,,,",
							freq[r], s[r]);
		}
	}

	//*********************************************************************
	//**                     Check Command Line Arguments               ***
	//*********************************************************************
	public int CheckArguments(string[] args, ref string inputFile, 
		ref string outputPath, ref string dataSetName, ref int fourMixed)
    {
        //args[0] inputfile
        //args[1] output path
        //args[2] switch for calculating 4 mixed
        //no argument or a 1 = calculate 4 mixed; 0 = don't calculate 

        //check for right number of arguments
        if (args.Length < 2 || args.Length > 3)
        {
            Console.WriteLine("{0} arguments instead of 2 or 3 arguments",
                args.Length);
            return 1;
        }

		inputFile = args[0];

		//determine DataSet Name to be used for output
		int startName = inputFile.LastIndexOf((slash)) + 1;
		int endName = inputFile.LastIndexOf(".");
        if (endName < 1)
            endName = inputFile.Length;
		dataSetName = inputFile.Substring(startName, (endName - startName));

		outputPath = args[1] + slash;		

		Console.WriteLine("input File:     {0}", inputFile);
		Console.WriteLine("output path:    {0}", outputPath);

        //check for flag for 4 mixed exp, set default if not indicated
        if (args.Length == 2)
            fourMixed = 1;
        else if (args.Length == 3)
            fourMixed = Convert.ToInt32(args[2]);

		return 0;
	}

	//*********************************************************************
	//***                    ChiSq Statistics                           ***
	//*********************************************************************
	public double ChiSq(int r, double[] fitsCount, int modelNum)
	{
		//calculate chisq, no binning
		double chiSq  = 0.0;
		double sumFit = 0.0;
		int rr = 1;
		for (int t = 1; t <= freq[r]; t++)
		{
			if (t == freq[rr])
			{
				chiSq += ((observedCount[rr] - fitsCount[t]) *
							 (observedCount[rr] - fitsCount[t]) / fitsCount[t]);

				rr++;
			}
			else
				chiSq += fitsCount[t];

			sumFit += fitsCount[t];
		}

        //Weighted Linear Regression doesn't use the tail
        if (modelNum < 6)
            chiSq += s[r] - sumFit;

		return chiSq;
	}


	//*********************************************************************
	//***               ChiSq Statistic with binning                    ***
	//*********************************************************************
	public double ChiSqBin(int r, double[] fitsExtended, int bin,
		ref int df, int numParams, ref int flag)
	{
		int extendedTau = freq[r] * 4;

		//find terminal indices of binned cells
		int[] check = new int[extendedTau + 1];
		double accumFit = 0.0;
		df = 0;
		int stop = 0;
		int t = 1;
		do
		{
			check[t] = 0;			
			accumFit += fitsExtended[t];

			if (accumFit >= bin && (s[r] - accumFit) >= bin)
			{
				check[t] = 1;
				df++;
				stop = t;
				accumFit = 0.0;
			}			
			t++;
		} while (t <= extendedTau && accumFit < bin && (s[r] - accumFit) >= bin);
			
		//check for enough data for binning and positive df
		double chiSq = 0.0;
		df = df - numParams;
		if (stop > 0 &&  df > 0)
		{
			double cellObs = 0.0;
			double cellFit = 0.0;
			int rr = 1;
			for (t = 1; t <= extendedTau; t++)
			{
				if (t <= freq[r])
				{					
					if (t == freq[rr])
					{
						cellObs += observedCount[rr];
						rr++;
					}
				}

				cellFit += fitsExtended[t];

				if (check[t] == 1)
				{
					chiSq += ((cellObs - cellFit) * (cellObs - cellFit) /
								 cellFit);
					cellObs = 0.0;
					cellFit = 0.0;
				}

			}

			//calculate tail of the distribution
			double observedTail = cellObs;
			double fitTail      = cellFit;

			chiSq += ((observedTail - fitTail) * (observedTail - fitTail)) /
				fitTail;
		}
		else
			flag = 0;

		return chiSq;
	}
    
    //*********************************************************************
    //**         Three Mixed Discounted Exponential Model               ***
    //*********************************************************************
    public double DiscountedFourToThreeMixedExponentialModel(int r,
        double[] T, ref double sHatTotal, ref double SE, ref double LCB,
        ref double UCB)
    {
        //estimated total species	        
        double cStar = s[r] * (1.0 - (T[4] * T[0]) / ((1.0 + T[0]) *
            (1.0 - (T[4] / (1.0 + T[0])) - (T[5] / (1.0 + T[1])) - (T[6] / (1.0 + T[2])) -
            ((1.0 - T[4] - T[5] - T[6]) / (1.0 + T[3])))));

        double excess = s[obsMax] - s[r];
        double sHatSubset = sHatTotal - excess;
        sHatSubset = (1.0 - T[4]) * sHatSubset;
        sHatTotal = sHatSubset + excess;

        //Standard Error
        SE = (1.0 - T[4]) * SE;

        //Confidence Bounds
        GetConfidenceBoundsDiscounted(r, SE, sHatSubset,
            cStar, excess, ref LCB, ref UCB);

        return cStar;
    }
    
    //*********************************************************************
    //**         Two Mixed Discounted Exponential Model                 ***
    //*********************************************************************
    public double DiscountedThreeToTwoMixedExponentialModel(int r, double[] T,
        ref double sHatTotal, ref double SE, ref double LCB, ref double UCB)
    {      
        //estimated total species			
        double cStar = s[r] * (1.0 - (T[3] * T[0]) / ((1.0 + T[0]) *
            (1.0 - (T[3] / (1.0 + T[0])) - (T[4] / (1.0 + T[1])) -
            ((1.0 - T[3] - T[4]) / (1.0 + T[2])))));

        double excess = s[obsMax] - s[r];
        double sHatSubset = sHatTotal - excess;
        sHatSubset = (1.0 - T[3]) * sHatSubset;
        sHatTotal = sHatSubset + excess;

        //Standard Error
        SE = (1.0 - T[3]) * SE;

        //Confidence Bounds
        GetConfidenceBoundsDiscounted(r, SE, sHatSubset,
            cStar, excess, ref LCB, ref UCB);

        return cStar;

    }

    //*********************************************************************
    //**        Single Discounted Exponential Model                 ***
    //*********************************************************************
    public double DiscountedTwoToSingleExponentialModel(int r, double[] T,
        ref double sHatTotal, ref double SE, ref double LCB, ref double UCB)
    {

        //estimated total species				
        double cStar = s[r] * (1.0 - (T[2] * T[0]) / ((1.0 + T[0]) *
           (1.0 - (T[2] / (1.0 + T[0])) - ((1.0 - T[2]) / (1.0 + T[1])))));

        double excess = s[obsMax] - s[r];
        double sHatSubset = sHatTotal - excess;
        sHatSubset = (1.0 - T[2]) * sHatSubset;
        sHatTotal = sHatSubset + excess;

        //Standard Error
        SE = (1.0 - T[2]) * SE;

        //Confidence Bounds
        GetConfidenceBoundsDiscounted(r, SE, sHatSubset,
            cStar, excess, ref LCB, ref UCB);

        return cStar;
    }
    
    //*********************************************************************
	//**             Four Mixed Exponential Fits                        ***
	//*********************************************************************
	public int FourMixedExponentialFits(int r, ref double u1,
		ref double u2, ref double u3, ref double mlesSExp1,
		ref double mlesSExp2, ref double mlesSExp3, ref double mlesSExp4,
		ref double mlesSExp5, ref double mlesSExp6, ref  double mlesSExp7,
		ref int fitsCheck, ref double[] fitsCount)
	{
		int MLEFlag = MLEFourMixedExp(r, ref u1, ref u2, ref u3,
				ref mlesSExp1, ref mlesSExp2, ref mlesSExp3, ref mlesSExp4);

		if (MLEFlag == 1)
		{
			double denom = (u3 * mlesSExp4 * mlesSExp1 * mlesSExp2)
							 + (u2 * mlesSExp4 * mlesSExp1 * mlesSExp3)
                             + (mlesSExp4 * mlesSExp1 * mlesSExp2 * mlesSExp3) 
							 + (u1 * mlesSExp4 * mlesSExp2 * mlesSExp3) 
                             + (mlesSExp1 * mlesSExp2 * mlesSExp3)
							 - (u3 * mlesSExp1 * mlesSExp2 * mlesSExp3) 
							 - (u1 * mlesSExp1 * mlesSExp2 * mlesSExp3) 
							 - (u2 * mlesSExp1 * mlesSExp2 * mlesSExp3);

			mlesSExp5 = ((1.0 + mlesSExp1) * u1 * mlesSExp4 * mlesSExp2 * mlesSExp3) /
							denom;

			mlesSExp6 = ((1.0 + mlesSExp2) * u2 * mlesSExp4 * mlesSExp1 * mlesSExp3) /
							denom;

			mlesSExp7 = ((1.0 + mlesSExp3) * u3 * mlesSExp4 * mlesSExp1 * mlesSExp2) /
							denom;

			fitsCheck = 1;
			for (int t = 1; t <= freq[r]; t++)
			{
				fitsCount[t] = s[r] *
					((u1 * ((1.0 / mlesSExp1) * Math.Pow((mlesSExp1 / (1.0 + mlesSExp1)), t))) +
					 (u2 * ((1.0 / mlesSExp2) * Math.Pow((mlesSExp2 / (1.0 + mlesSExp2)), t))) +
					 (u3 * ((1.0 / mlesSExp3) * Math.Pow((mlesSExp3 / (1.0 + mlesSExp3)), t))) +
					((1.0 - u1 - u2 - u3) * ((1.0 / mlesSExp4) *
					Math.Pow((mlesSExp4 / (1.0 + mlesSExp4)), t))));

				//if fits is negative, throw flag for bad fits
				if (fitsCount[t] < 0.0)
					fitsCheck = 0;
			}
		}

		return MLEFlag;
	}

    //*********************************************************************
    //**            Four Mixed Exponential Model                        ***
    //*********************************************************************
    public void FourMixedExponentialModel(string modelName,
        StreamWriter swAnalysis, int fMin, int numParams, int modelNum, 
        double maxGOF, int[] bestCount)
    {
        string[] T = new string[7];

        //find fmin frequency
        int r = 1;
        while (freq[r] < fMin)
            r++;
        int freqMin = r;

        for (r = freqMin; r <= obsMax; r++)
        {
            double u1 = 0.0;
            double u2 = 0.0;
            double u3 = 0.0;

            double mlesSExp1 = 0.0;
            double mlesSExp2 = 0.0;
            double mlesSExp3 = 0.0;
            double mlesSExp4 = 0.0;
            double mlesSExp5 = 0.0;
            double mlesSExp6 = 0.0;
            double mlesSExp7 = 0.0;

            int fitsCheck = 1;

            double[] fitsCount = new double[freq[r] + 1];

            int MLEFlag = FourMixedExponentialFits(r, ref u1, ref u2, ref u3,
                ref  mlesSExp1, ref  mlesSExp2, ref mlesSExp3, ref mlesSExp4,
                ref  mlesSExp5, ref  mlesSExp6, ref  mlesSExp7, ref  fitsCheck,
                ref fitsCount);

            if (MLEFlag == 1 && fitsCheck == 1)
            {
                //compute fitted values 
                int extendedTau = freq[r] * 4;
                double[] fitsExtended = new double[extendedTau + 1];

                for (int t = 1; t <= freq[r]; t++)
                    fitsExtended[t] = fitsCount[t];

                //compute extended fitted values 
                for (int t = (freq[r] + 1); t <= extendedTau; t++)
                    fitsExtended[t] = s[r] *
                    ((u1 * ((1.0 / mlesSExp1) * Math.Pow((mlesSExp1 / (1.0 + mlesSExp1)), t))) +
                     (u2 * ((1.0 / mlesSExp2) * Math.Pow((mlesSExp2 / (1.0 + mlesSExp2)), t))) +
                     (u3 * ((1.0 / mlesSExp3) * Math.Pow((mlesSExp3 / (1.0 + mlesSExp3)), t))) +
                    ((1.0 - u1 - u2 - u3) * ((1.0 / mlesSExp4) *
                    Math.Pow((mlesSExp4 / (1.0 + mlesSExp4)), t))));

                double sHatSubset = s[r] * (((1.0 + mlesSExp1) * (1.0 + mlesSExp2) *
                    (1.0 + mlesSExp3) * (1.0 + mlesSExp4)) / ((mlesSExp5 * mlesSExp1 * mlesSExp3) +
                    (mlesSExp4 * mlesSExp2 * mlesSExp3) - (mlesSExp7 * mlesSExp2 * mlesSExp4) +
                    (mlesSExp6 * mlesSExp2 * mlesSExp3) - (mlesSExp7 * mlesSExp4 * mlesSExp1) +
                    (mlesSExp6 * mlesSExp1 * mlesSExp2) + (mlesSExp4 * mlesSExp1 * mlesSExp2) -
                    (mlesSExp5 * mlesSExp2 * mlesSExp4) - (mlesSExp5 * mlesSExp4 * mlesSExp3) +
                    (mlesSExp1 * mlesSExp3 * mlesSExp4) + (mlesSExp7 * mlesSExp2 * mlesSExp3) -
                    (mlesSExp6 * mlesSExp4 * mlesSExp1) + (mlesSExp7 * mlesSExp1 * mlesSExp3) -
                    (mlesSExp7 * mlesSExp4) + (mlesSExp5 * mlesSExp1) +
                    (mlesSExp6 * mlesSExp2) + mlesSExp4 + (mlesSExp4 * mlesSExp3) +
                    (mlesSExp7 * mlesSExp3) + (mlesSExp2 * mlesSExp4) -
                    (mlesSExp5 * mlesSExp4) - (mlesSExp6 * mlesSExp4) +
                    (mlesSExp7 * mlesSExp1 * mlesSExp2 * mlesSExp3) +
                    (mlesSExp1 * mlesSExp2 * mlesSExp3 * mlesSExp4) -
                    (mlesSExp6 * mlesSExp4 * mlesSExp3) + (mlesSExp5 * mlesSExp1 * mlesSExp2) +
                    (mlesSExp4 * mlesSExp1) - (mlesSExp6 * mlesSExp1 * mlesSExp3 * mlesSExp4) -
                    (mlesSExp5 * mlesSExp4 * mlesSExp2 * mlesSExp3) +
                    (mlesSExp6 * mlesSExp1 * mlesSExp2 * mlesSExp3) +
                    (mlesSExp5 * mlesSExp1 * mlesSExp2 * mlesSExp3) -
                    (mlesSExp7 * mlesSExp4 * mlesSExp1 * mlesSExp2)));


                double sHatTotal = sHatSubset + (s[obsMax] - s[r]);

                double part1 = lnSFactorial[r] - sumlnFFactorial[r];

                double part2 = 0.0;
                for (int t = 1; t <= r; t++)
                {
                    part2 += (observedCount[t] * Math.Log(
                    (u1 * ((1.0 / mlesSExp1) * Math.Pow((mlesSExp1 / (1.0 + mlesSExp1)), freq[t]))) +
                    (u2 * ((1.0 / mlesSExp2) * Math.Pow((mlesSExp2 / (1.0 + mlesSExp2)), freq[t]))) +
                    (u3 * ((1.0 / mlesSExp3) * Math.Pow((mlesSExp3 / (1.0 + mlesSExp3)), freq[t]))) +
                    ((1.0 - u1 - u2 - u3) * ((1.0 / mlesSExp4) *
                    Math.Pow((mlesSExp4 / (1.0 + mlesSExp4)), freq[t])))));
                }

                double AIC = 0.0;
                double AICc = 0.0;
                double chiSqAll = 0.0;
                double GOF0 = 0.0;
                double GOF5 = 0.0;

                int AICcCheck = 0;
                int GOF0Check = 0;
                int GOF5Check = 0;

                GOF5Test[modelNum, r] = CalculateAnalysisVariables(part1, part2,
                    numParams, r, fitsCount, fitsExtended, maxGOF, ref AIC,
                    ref AICc, ref AICcCheck, ref chiSqAll, ref GOF0, ref GOF5,
                    ref GOF0Check, ref GOF5Check, modelNum);

                //Standard Error
                int SEFlag = 0;

                double SE = SEFourMixedExp(mlesSExp1, mlesSExp2, mlesSExp3,
                    mlesSExp4, mlesSExp5, mlesSExp6, mlesSExp7, sHatSubset, ref SEFlag);

                //Confidence Bounds
                double LCB = 0.0;
                double UCB = 0.0;
                int boundsCheck = 0;
                if (SEFlag == 1)
                    boundsCheck = GetConfidenceBounds(r, SE, sHatSubset,
                        ref LCB, ref UCB);

                //Write out Analysis row for this dataset/model/tau
                T[0] = mlesSExp1.ToString();
                T[1] = mlesSExp2.ToString();
                T[2] = mlesSExp3.ToString();
                T[3] = mlesSExp4.ToString();
                T[4] = mlesSExp5.ToString();
                T[5] = mlesSExp6.ToString();
                T[6] = mlesSExp7.ToString();

                WriteAnalysisRow(swAnalysis, modelName, r, sHatTotal,
                        SEFlag, SE, boundsCheck, LCB, UCB, GOF0Check, GOF0,
                        GOF5Check, GOF5, AICcCheck, AICc, T, chiSqAll, AIC,
                        modelNum, bestCount);
            }
            else
                //Write out Analysis row as all missing
                swAnalysis.WriteLine("{0},{1},{2},,,,,,,,,,,,,,,,",
                    modelName, freq[r], s[r]);
        }
    }
    
    
    //*********************************************************************
	//***              Confidence Bounds                                ***
	//*********************************************************************
	public int GetConfidenceBounds(int r, double SE, double sHatSubset,
		ref double LCB, ref double UCB)
	{
		//Confidence Bounds  
		if (sHatSubset != s[r])
		{
			double dTemp = Math.Exp(1.96 * (Math.Sqrt(Math.Log(1.0 + (SE * SE /
				(((sHatSubset - s[r]) * (sHatSubset - s[r]))))))));
			LCB = s[obsMax] + ((sHatSubset - s[r]) / dTemp);
			UCB = s[obsMax] + ((sHatSubset - s[r]) * dTemp);
			return 1;
		}
		else
			return 0;
	}

    //*********************************************************************
    //***              Confidence Bounds                                ***
    //*********************************************************************
    public void GetConfidenceBoundsDiscounted(int r, double SE, 
        double sHatSubset, double cStar, double excess, ref double LCB, 
        ref double UCB)
    {
        //Confidence Bounds  
        if (sHatSubset != s[r])
        {
            double dTemp = Math.Exp(1.96 * (Math.Sqrt(Math.Log(1.0 + (SE * SE /
                (((sHatSubset - cStar) * (sHatSubset - cStar))))))));
            LCB = (cStar + excess) + ((sHatSubset - cStar) / dTemp);
            UCB = (cStar + excess) + ((sHatSubset - cStar) * dTemp);
        }
    }
    
    //*********************************************************************
	//***              Mles for Poisson Model                           ***
	//*********************************************************************
	public static double GetPoissonMles(double poissonConstant, double momentsInit,
		 ref int flag)
	{
		// maximum iterations for Poisson non-zero-truncated max likelihood search
		int mleNonztIter = 10000;

		// convergence tolerance for Poisson non-zero-truncated max likelihood search
		double mleNonztTol = 6.0;
		mleNonztTol = Math.Pow(10.0, -mleNonztTol);

		double x1 = 0.0;
		double x2 = 0.0;

		double f1 = 0.0;
		double f2 = 0.0;

		int success = BracetRoot(poissonConstant, momentsInit,
			ref x1, ref x2, ref f1, ref f2);

		if (success == 1)
		{
			double root = 0.0;
			double dx = 0.0;

			if (f1 < 0.0)
			{
				root = x1;
				dx = x2 - x1;
			}
			else
			{
				root = x2;
				dx = x1 - x2;
			}

			//Bisection loop
			for (int i = 0; i < mleNonztIter; i++)
			{
				dx *= 0.5;

				double xmid = root + dx;

				f2 = (xmid / (1.0 - Math.Exp(-xmid))) - poissonConstant;

				if (f2 <= 0.0)
					root = xmid;

				if (Math.Abs(dx) < mleNonztTol || f2 == 0.0)
					return root;
			}
			Console.WriteLine("Bisection failed for Poisson mles");

			flag = 0;

			return 0.0;
		}
		else
		{
			Console.WriteLine("no convergence for Poisson mles");

			flag = 0;

			return 0.0;
		}
	}

	
	//*********************************************************************
	//***              Goodness of Fit                                  ***
	//*********************************************************************
	public static double GoodNessOfFit(double chiSqAll, int df, ref int flag)
	{
		double v = df * 0.5;
		double x = chiSqAll * 0.5;
		double g = 1.0;
		double p = 1.0;

		v += 1.0;
		while (v <= 2.0)
		{
			g = g * x;
			p = p * v + g;
			v += 1.0;
		} 

		int j = Convert.ToInt32(Math.Floor(((5.0 * (3 + Math.Abs(x)) * 0.5))));
		double f = 1.0 / (j + v - x);

		for (int jj = (j - 1); jj >=0; jj--)
			f = (f * x + 1.0) / (jj + v);

		p += (f * g * x);
		g = (1.0 - ((2.0 / (7.0 * v * v)) * (1.0 - (2.0 / (3.0 * v * v)))))/ 
			(30.0 * v * v);
		g = ((g - 1.0) / (12.0 * v)) - (v * (Math.Log(v) - 1.0));
		f = p * Math.Exp(g - x) * Math.Sqrt(v / (2.0 * Math.PI));

		if (Double.IsNaN(f))
			flag = 0;

		double GOF = (1.0 - (Math.Pow((chiSqAll * 0.5), (df * 0.5)) *
			f));

		if (Double.IsNaN(GOF) || Double.IsInfinity(GOF) ||
			Double.IsNegativeInfinity(GOF) || Double.IsPositiveInfinity(GOF))
			flag = 0;

		return GOF;
	}

	//*********************************************************************
	//**                      Good Turing Model                         ***
	//*********************************************************************
	public void GoodTuringModel(StreamWriter swAnalysis, int freqMin,
		double[] GTEstimate, double singletons)
	{
		//f1
		double f1 = singletons;

		for (int r = freqMin; r <= obsMax; r++)
		{		
			//estimate of total species
			double sHatTotal = GTEstimate[r] + (s[obsMax] - s[r]);

			//Standard Error
			double temp1 = (n[r] + s[r]) / (n[r] - f1);

			double A = (temp1 * temp1) * f1 * (1.0 - (f1 / GTEstimate[r]));

			double B = 0.0;
			double C = 0.0;
			double D = 0.0;

			double[] temp2 = new double[freq[r] + 1];

			for (int i = 2; i <= freq[r]; i++)
				temp2[i] = (n[r] / (n[r] - f1)) - (i * s[r] * f1 /
					((n[r] - f1) * (n[r] - f1)));

			int rr = 1;
			while (freq[rr] < 2)
			{
				rr++;
			} 

			for (int i = 2; i <= freq[r]; i++)
			{
				if (i == freq[rr])
				{
					B += (temp2[i] * temp2[i] * observedCount[rr] *
						(1.0 - (observedCount[rr] / GTEstimate[r])));

					C += (temp1 * temp2[i] * (-f1 * observedCount[rr] /
						GTEstimate[r]));

					int rj = 1;
					while (freq[rj] < i + 1 && rj < r)
					{
						rj++;
					}

					for (int j = i + 1; j <= freq[r]; j++)
					{
						if (j == freq[rj])
						{
							D += (temp2[i] * temp2[j] * (-observedCount[rr] *
								observedCount[rj] / GTEstimate[r]));
							rj++;
						}
					}
					rr++;
				}
			}

			double SE = A + B + 2.0 * C + 2.0 * D;
			int SEFlag = 0;
			if (SE > 0.0)
			{
				SE = Math.Sqrt(SE);
				SEFlag = 1;
			}

			if (SEFlag == 1)
			{
				//Confidence Bounds
				double LCB = 0.0;
				double UCB = 0.0;
				GetConfidenceBounds(r, SE, GTEstimate[r], ref LCB, ref UCB);

				//Write out Analysis row for this dataset/model/tau
				swAnalysis.WriteLine("GoodTuring,{0},{1},{2},{3},{4},{5},,,,,,,,,,,,,",
					freq[r], s[r], sHatTotal, SE, LCB, UCB);
			}
			// Bad SE
			else
				swAnalysis.WriteLine("GoodTuring,{0},{1},{2},,,,,,,,,,,,,,,,",
					freq[r], s[r], sHatTotal);
		}
	}

	//*********************************************************************
	//***                       Input data                              ***
	//*********************************************************************
	public int InputDataFromFile(string inputFile)
	{
		FileInfo fi = new FileInfo(inputFile);
		StreamReader sr = new StreamReader(fi.OpenRead());

		Regex comma = new Regex(",");

		//determine max frequency, number of non-zero counts in file
		int numRows = 0;
		string freqData;
		while ((freqData = sr.ReadLine()) != null)
		{
			//data[0] = freq; data[1] = count
			string[] data = comma.Split(freqData);
			if (Convert.ToInt32(data[1]) > 0)
				numRows++;			
		}
		sr.Close();

		//Need at least two rows of data for analysis
		if (numRows > 1)
		{
			obsMax = numRows;

			freq          = new int[(obsMax + 1)];
			observedCount = new double[(obsMax + 1)];

			bestGOF0 = new double[4, 9, (obsMax + 1)];
			bestAICc = new double[4, 9, (obsMax + 1)];
			GOF5Test = new double[9, (obsMax + 1)];

			bubblePlotData     = new string[8, (obsMax + 1)];
			bubblePlotDataFlag = new int[8, (obsMax + 1)];

			StreamReader srData = new StreamReader(fi.OpenRead());

			//zero out input matrix
			for (int i = 0; i <= obsMax; i++)
			{
				freq[i] = 0;
				observedCount[i] = 0;
			}

			//fill in input matrix
			int r = 0;
			while ((freqData = srData.ReadLine()) != null)
			{
				//data[0] = freq; data[1] = count
				string[] data = comma.Split(freqData);

				if (Convert.ToInt32(data[1]) > 0)
				{
					r++;
					freq[r] = Convert.ToInt32(data[0]);
					observedCount[r] = Convert.ToInt32(data[1]);

					if (observedCount[r] > 0)
					{
						if (freq[r] <= 10)
							freqTau10 = r;
					}
				}	
			}
			srData.Close();
			return 1;
		}
		else
			return 0;
	}

    //*********************************************************************
    //**         Weighted Linear Regression Fits                        ***
    //*********************************************************************
    public void LogTWLRFits(int r, ref double gamma,
        ref double delta, ref double MSE, ref double k, ref int fitsCheck,
        ref double[] fitsCount)
    {
        //calculate k
        for (int j = 1; j < r; j++)
        {
            double temp = 0.0;
            for (int i = 1; i < r; i++)
                temp += i * lnW[i] * (i - j);
            k += lnW[j] * temp;
        }

        //calculate gamma
        for (int j = 1; j < r; j++)
        {
            double temp = 0.0;
            for (int i = 1; i < r; i++)
                temp += (j - i) * lnW[i] * lnY[i];
            gamma += j * lnW[j] * temp;
        }
        gamma /= k;

        //calculate delta
        for (int j = 1; j < r; j++)
        {
            double temp = 0.0;
            for (int i = 1; i < r; i++)
                temp += (i - j) * lnW[i] * lnY[i];
            delta += lnW[j] * temp;
        }
        delta /= k;

        //calculate MSE
        for (int j = 1; j < r; j++)
            MSE += (lnW[j] * (lnY[j] - gamma - delta * j) * (lnY[j] -
                gamma - delta * j));
        MSE *= (1.0 / (r - 3.0));

        fitsCount[0] = observedCount[freq[1]] * Math.Exp(-gamma);
        fitsCount[1] = observedCount[freq[1]];
        for (int t = 2; t <= r; t++)
        {
            fitsCount[t] = fitsCount[t - 1] *
                Math.Exp(gamma + delta * (t - 1.0)) / t;

            //if fits is negative, throw flag for bad fits
            if (fitsCount[t] < 0.0)
                fitsCheck = 0;
        }
    }

    //*********************************************************************
    //**            Weighted Linear Regression Model                    ***
    //*********************************************************************
    public void LogTWLRModel(string modelName, StreamWriter swAnalysis,
    int fMin, int numParams, int modelNum, double maxGOF,
    int[] bestCount)
    {
        string[] T = new string[7];

        //fmin frequency has to be fMin since frequencies have to be contiguous                
        int freqMin = fMin;

        for (int r = freqMin; r <= freqMax; r++)
        {
            double gamma = 0.0;
            double delta = 0.0;
            double MSE = 0.0;
            double k = 0.0;

            int fitsCheck = 1;

            double[] fitsCount = new double[freq[r] + 1];

            LogTWLRFits(r, ref gamma, ref delta, ref MSE, ref k,
                ref fitsCheck, ref fitsCount);

            //proceed if no negative fits calculated
            if (fitsCheck == 1)
            {
                //estimated total species			
                double sHatSubset = fitsCount[0] + s[r];

                double sHatTotal = sHatSubset + (s[obsMax] - s[r]);

                double AIC = 0.0;
                double AICc = 0.0;

                //calculate Goodness of Fit
                double chiSqAll = ChiSq(r, fitsCount, modelNum);

                int df = freq[r] - numParams;
                int flag = 1;
                double GOF0 = 0.0;

                double test = (chiSqAll - df) / Math.Sqrt(2.0 * df);
                if (test < maxGOF && chiSqAll < BigChiSq)
                    GOF0 = GoodNessOfFit(chiSqAll, df, ref flag);
                else
                    flag = 0;

                //WLRM Switch revert to logged version if it's GOF0 is smaller
                int GOF0Check = flag;
                WLRMGOF0[freq[r]] = GOF0;

                int GOF5Check = 0;
                int AICcCheck = 0;

                GOF5Test[modelNum, r] = 0.0;
                double GOF5 = 0.0;

                //Standard Error
                double varGamma = 0.0;
                for (int i = 1; i < r; i++)
                    varGamma += (i * i * lnW[i]);
                varGamma *= MSE / k;

                double SE = (s[r] * fitsCount[0] / sHatSubset) + (Math.Exp(-2.0 * gamma) *
                    observedCount[freq[1]] * (varGamma * observedCount[freq[1]] + 1.0));
                int SEFlag = 0;

                if (SE > 0.0)
                {
                    SE = Math.Sqrt(SE);
                    SEFlag = 1;
                }

                //Confidence Bounds
                double LCB = 0.0;
                double UCB = 0.0;
                int boundsCheck = 0;

                if (SEFlag == 1)
                    boundsCheck = GetConfidenceBounds(r, SE, sHatSubset,
                        ref LCB, ref UCB);

                //Write out Analysis row for this dataset/model/tau
                T[0] = gamma.ToString();
                T[1] = delta.ToString();
                T[2] = MSE.ToString();
                WriteAnalysisRow(swAnalysis, modelName, r, sHatTotal,
                    SEFlag, SE, boundsCheck, LCB, UCB, GOF0Check, GOF0,
                    GOF5Check, GOF5, AICcCheck, AICc, T, chiSqAll, AIC,
                    modelNum, bestCount);
            }
            else
                //Write out Analysis row as all missing
                swAnalysis.WriteLine("{0},{1},{2},,,,,,,,,,,,,,,,,,",
                    modelName, freq[r], s[r]);
        }
    }


	//*********************************************************************
	//**                        back substitution                       ***
	//*********************************************************************
	public static void Lubksb(double[,] a, int n, int[] indx, double[] b)
	{
		int ii = 0;

		for (int i = 1; i <= n; i++)
		{
			int ip = indx[i];
			double sum = b[ip];
			b[ip] = b[i];
			if (ii > 0)
			{
				for (int j = ii; j <= i - 1; j++)
					sum -= a[i, j] * b[j];
			}
			else if (sum > 0)
				ii = i;

			b[i] = sum;
		}
		for (int i = n; i >= 1; i--)
		{
			double sum = b[i];
			for (int j = i + 1; j <= n; j++)
				sum -= a[i, j] * b[j];

			b[i] = sum / a[i, i];
		}
	}


	//*********************************************************************
	//**                        LU Decomposition                        ***
	//*********************************************************************
	public static void Ludcmp(double[,] a, int n, int[] indx)
	{
        double TINY = Math.Exp(-20.0);

		double[] vv = new double[(n + 1)];

		for (int i = 1; i <= n; i++)
		{
			double big = 0.0;
			for (int j = 1; j <= n; j++)
			{
				if ((Math.Abs(a[i, j])) > big)
					big = Math.Abs(a[i, j]);

				if (big == 0.0)
					Console.WriteLine("Singular matrix in routine ludcmp");

				vv[i] = 1.0 / big;
			}
		}

		for (int j = 1; j <= n; j++)
		{
			for (int i = 1; i < j; i++)
			{
				double sum = a[i, j];
				for (int k = 1; k < i; k++)
					sum -= a[i, k] * a[k, j];

				a[i, j] = sum;
			}

			double big = 0.0;
			int imax = 0;
			for (int i = j; i <= n; i++)
			{
				double sum = a[i, j];
				for (int k = 1; k < j; k++)
					sum -= a[i, k] * a[k, j];

				a[i, j] = sum;

				double dum = vv[i] * Math.Abs(sum);
				if (dum >= big)
				{
					big = dum;
					imax = i;
				}
			}

			if (j != imax)
			{
				for (int k = 1; k <= n; k++)
				{
					double dum = a[imax, k];
					a[imax, k] = a[j, k];
					a[j, k] = dum;
				}
				vv[imax] = vv[j];
			}

			indx[j] = imax;
			if (a[j, j] == 0.0)
				a[j, j] = TINY;

			if (j != n)
			{
				double dum = 1.0 / (a[j, j]);
				for (int i = j + 1; i <= n; i++)
					a[i, j] *= dum;
			}
		}
	}

    //*********************************************************************
    //**                 Invert Matrix for SE                           ***
    //*********************************************************************
    public static double MatrixInversion(int n, double sHatSubset, double a00,
        double[] a0, double[,] A, ref int SEFlag)
    {			
        /* LU Decomposition */
		int[] indx = new int[n + 1];

        /* Complete symetric A matrix */
        for (int r = 2; r <= n; r++)
        for (int c = 1; c <= n; c++)
        {
            if (c < r)
                A[r, c] = A[c, r];
        }

		Ludcmp(A, n, indx);

		/* Invert matrix */
		double[] col = new double[(n + 1)];
		double[,] AInverse = new double[(n + 1), (n + 1)];

		for (int j = 1; j <= n; j++)
		{
			for (int i = 1; i <= n; i++)
				col[i] = 0.0;

			col[j] = 1.0;

			Lubksb(A, n, indx, col);

			for (int i = 1; i <= n; i++)
				AInverse[i, j] = col[i];
		}

		double[] resultMatrix = new double[(n + 1)];
		for (int c = 1; c <= n; c++)
			for (int r = 1; r <= n; r++)
				resultMatrix[c] += a0[r] * AInverse[r, c];

		double answer = 0.0;
		for (int i = 1; i <= n; i++)
			answer += resultMatrix[i] * a0[i];

		// check for positive sqrt; leave SE = 0 otherwise
		double SE = 0.0;
		if (a00 > answer)
		{
			answer = Math.Sqrt(a00 - answer);
			SE = Math.Sqrt(sHatSubset) / answer;
			SEFlag = 1;
		}

		return SE;
    }

    //*********************************************************************
	//**                 MLE Four Mixed Expontial                       ***
	//*********************************************************************
	public int MLEFourMixedExp(int r, ref double u1, ref double u2,
		ref double u3, ref double t1, ref double t2, ref double t3,
		ref double t4)
	{
		// find initial maximum likelihood estimators
		u1 = 0.25;
		u2 = 0.25;
		u3 = 0.25;

		int k = Convert.ToInt32(freq[r] * 0.4);
		int r1 = 1;
		while (freq[r1] <= k)
			r1++;
		r1--;
		int k1 = 1;
		while (freq[k1] < freq[r1])
			k1++;

		k = Convert.ToInt32(freq[r] * 0.2);
		int r2 = 1;
		while (freq[r2] <= k)
			r2++;
		r2--;
		int k2 = 1;
		while (freq[k2] < freq[r2])
			k2++;

		k = Convert.ToInt32(freq[r] * 0.6);
		int r3 = 1;
		while (freq[r3] <= k)
			r3++;
		r3--;
		int k3 = 1;
		while (freq[k3] < freq[r3])
			k3++;

		k = Convert.ToInt32(freq[r] * 0.8);
		int r4 = 1;
		while (freq[r4] <= k)
			r4++;
		r4--;
		int k4 = 1;
		while (freq[k4] < freq[r4])
			k4++;

		if ((n[k1] != s[k1]) && (s[k3] != s[k2]) && (s[k4] != s[k2]))
		{
			t1 = (n[k1] / s[k1]) - 1.0;
			t2 = ((n[k3] - n[k2]) / (s[k3] - s[k2])) - 1.0;
			t3 = ((n[k4] - n[k2]) / (s[k4] - s[k2])) - 1.0;
			t4 = ((n[r] - n[k3]) / (s[r] - s[k3])) - 1.0;

			double part2 = 0.0;
			for (int t = 1; t <= r; t++)
				part2 += (observedCount[t] * Math.Log(
					(u1 * ((1.0 / t1) * Math.Pow((t1 / (1.0 + t1)), freq[t]))) +
					(u2 * ((1.0 / t2) * Math.Pow((t2 / (1.0 + t2)), freq[t]))) +
					(u3 * ((1.0 / t3) * Math.Pow((t3 / (1.0 + t3)), freq[t]))) +
					((1.0 - u1 - u2 - u3) * ((1.0 / t4) * Math.Pow((t4 / (1.0 + t4)), freq[t])))));

			double tolerance = Math.Pow(10.0, -10.0);
			double part2Old = part2;
			double deltaPart2 = 0.0;
			k = 0;
			do
			{
				double[] z1 = new double[r + 1];
				double[] z2 = new double[r + 1];
				double[] z3 = new double[r + 1];

				for (int t = 1; t <= r; t++)
				{
					double denom = ((u1 * (1.0 / t1) * Math.Pow((t1 / (1.0 + t1)), freq[t])) +
										 (u2 * (1.0 / t2) * Math.Pow((t2 / (1.0 + t2)), freq[t])) +
										 (u3 * (1.0 / t3) * Math.Pow((t3 / (1.0 + t3)), freq[t])) +
					   ((1.0 - u1 - u2 - u3) * (1.0 / t4) * Math.Pow((t4 / (1.0 + t4)), freq[t])));

					z1[t] = (u1 * (1.0 / t1) * Math.Pow((t1 / (1.0 + t1)), freq[t])) / denom;
					z2[t] = (u2 * (1.0 / t2) * Math.Pow((t2 / (1.0 + t2)), freq[t])) / denom;
					z3[t] = (u3 * (1.0 / t3) * Math.Pow((t3 / (1.0 + t3)), freq[t])) / denom;
				}

				u1 = 0.0;
				u2 = 0.0;
				u3 = 0.0;
				t1 = 0.0;
				t2 = 0.0;
				t3 = 0.0;
				t4 = 0.0;

				double t1Part1 = 0.0;
				double t1Part2 = 0.0;

				double t2Part1 = 0.0;
				double t2Part2 = 0.0;

				double t3Part1 = 0.0;
				double t3Part2 = 0.0;

				double t4Part1 = 0.0;
				double t4Part2 = 0.0;

				for (int t = 1; t <= r; t++)
				{
					u1 += (observedCount[t] * z1[t]);
					u2 += (observedCount[t] * z2[t]);
					u3 += (observedCount[t] * z3[t]);

					t1Part1 += observedCount[t] * freq[t] * z1[t];
					t2Part1 += observedCount[t] * freq[t] * z2[t];
					t3Part1 += observedCount[t] * freq[t] * z3[t];
					t4Part1 += observedCount[t] * freq[t] * (1.0 - z1[t] - z2[t] - z3[t]);

					t1Part2 += observedCount[t] * z1[t];
					t2Part2 += observedCount[t] * z2[t];
					t3Part2 += observedCount[t] * z3[t];
					t4Part2 += observedCount[t] * (1.0 - z1[t] - z2[t] - z3[t]);
				}

				u1 *= (1.0 / s[r]);
				u2 *= (1.0 / s[r]);
				u3 *= (1.0 / s[r]);

				t1 = (t1Part1 / t1Part2) - 1.0;
				t2 = (t2Part1 / t2Part2) - 1.0;
				t3 = (t3Part1 / t3Part2) - 1.0;
				t4 = (t4Part1 / t4Part2) - 1.0;


				part2 = 0.0;
				for (int t = 1; t <= r; t++)
					part2 += (observedCount[t] * Math.Log(
					(u1 * ((1.0 / t1) * Math.Pow((t1 / (1.0 + t1)), freq[t]))) +
					(u2 * ((1.0 / t2) * Math.Pow((t2 / (1.0 + t2)), freq[t]))) +
					(u3 * ((1.0 / t3) * Math.Pow((t3 / (1.0 + t3)), freq[t]))) +
					((1.0 - u1 - u2 - u3) * ((1.0 / t4) * Math.Pow((t4 / (1.0 + t4)), freq[t])))));


				deltaPart2 = (part2 - part2Old);

				part2Old = part2;

				k++;
			}
			while (deltaPart2 > tolerance);

			if (Double.IsNaN(part2))
				return 0;
			else
				return 1;
		}
		else
			return 0;
	}
    
	//*********************************************************************
	//**                 MLE Three Mixed Expontial                      ***
	//*********************************************************************
	public int MLEThreeMixedExp(int r, ref double u1, ref double u2,
		ref double t1, ref double t2, ref double t3)
	{
		// find initial maximum likelihood estimators
		u1 = 0.33;
		u2 = 0.33;

		int k = Convert.ToInt32(freq[r] * 0.5);
		int r1 = 1;
		while (freq[r1] <= k)
			r1++;
		r1--;
		int k1 = 1;
		while (freq[k1] < freq[r1])
			k1++;

		k = Convert.ToInt32(freq[r] * 0.25);
		int r2 = 1;
		while (freq[r2] <= k)
			r2++;
		r2--;
		int k2 = 1;
		while (freq[k2] < freq[r2])
			k2++;

		k = Convert.ToInt32(freq[r] * 0.75);
		int r3 = 1;
		while (freq[r3] <= k)
			r3++;
		r3--;
		int k3 = 1;
		while (freq[k3] < freq[r3])
			k3++;

		if ((n[k1] != s[k1]) && (s[k3] != s[k2]))
		{
			t1 = (n[k1] / s[k1]) - 1.0;
			t2 = ((n[k3] - n[k2]) / (s[k3] - s[k2])) - 1.0;
			t3 = ((n[r] - n[k1]) / (s[r] - s[k1])) - 1.0;

			double part2 = 0.0;
			for (int t = 1; t <= r; t++)
				part2 += (observedCount[t] * Math.Log(
					(u1 * ((1.0 / t1) * Math.Pow((t1 / (1.0 + t1)), freq[t]))) +
					(u2 * ((1.0 / t2) * Math.Pow((t2 / (1.0 + t2)), freq[t]))) +
					((1.0 - u1 - u2) * ((1.0 / t3) * Math.Pow((t3 / (1.0 + t3)), freq[t])))));

			double tolerance = Math.Pow(10.0, -10.0);
			double part2Old = part2;
			double deltaPart2 = 0.0;
			k = 0;
			do
			{
				double[] z1 = new double[r + 1];
				double[] z2 = new double[r + 1];

				for (int t = 1; t <= r; t++)
				{
					double denom = ((u1 * (1.0 / t1) * Math.Pow((t1 / (1.0 + t1)), freq[t])) +
						 (u2 * (1.0 / t2) * Math.Pow((t2 / (1.0 + t2)), freq[t])) +
						((1.0 - u1 - u2) * (1.0 / t3) * Math.Pow((t3 / (1.0 + t3)), freq[t])));

					z1[t] = (u1 * (1.0 / t1) * Math.Pow((t1 / (1.0 + t1)), freq[t])) / denom;

					z2[t] = (u2 * (1.0 / t2) * Math.Pow((t2 / (1.0 + t2)), freq[t])) / denom;
				}

				u1 = 0.0;
				u2 = 0.0;
				t1 = 0.0;
				t2 = 0.0;
				t3 = 0.0;

				double t1Part1 = 0.0;
				double t1Part2 = 0.0;

				double t2Part1 = 0.0;
				double t2Part2 = 0.0;

				double t3Part1 = 0.0;
				double t3Part2 = 0.0;

				for (int t = 1; t <= r; t++)
				{
					u1 += (observedCount[t] * z1[t]);
					u2 += (observedCount[t] * z2[t]);

					t1Part1 += observedCount[t] * freq[t] * z1[t];
					t2Part1 += observedCount[t] * freq[t] * z2[t];
					t3Part1 += observedCount[t] * freq[t] * (1.0 - z1[t] - z2[t]);

					t1Part2 += observedCount[t] * z1[t];
					t2Part2 += observedCount[t] * z2[t];
					t3Part2 += observedCount[t] * (1.0 - z1[t] - z2[t]);
				}

				u1 *= (1.0 / s[r]);
				u2 *= (1.0 / s[r]);

				t1 = (t1Part1 / t1Part2) - 1.0;
				t2 = (t2Part1 / t2Part2) - 1.0;
				t3 = (t3Part1 / t3Part2) - 1.0;

				part2 = 0.0;
				for (int t = 1; t <= r; t++)
					part2 += (observedCount[t] * Math.Log(
					(u1 * ((1.0 / t1) * Math.Pow((t1 / (1.0 + t1)), freq[t]))) +
					(u2 * ((1.0 / t2) * Math.Pow((t2 / (1.0 + t2)), freq[t]))) +
					((1.0 - u1 - u2) * ((1.0 / t3) * Math.Pow((t3 / (1.0 + t3)), freq[t])))));


				deltaPart2 = (part2 - part2Old);

				part2Old = part2;

				k++;
			}
			while (deltaPart2 > tolerance);

			if (Double.IsNaN(part2))
				return 0;
			else
				return 1;
		}
		else
			return 0;
	}

	//*********************************************************************
	//**                 MLE Two Mixed Expontial                        ***
	//*********************************************************************
	public int MLETwoMixedExp(int r, ref double u, ref double t1,
		ref double t2)
	{
		// initial maximum likelihood estimators
		u = 0.5;
		int k = Convert.ToInt32(freq[r] * 0.67);
		int r1 = 1;
		while (freq[r1] <= k)
			r1++;
		r1--;
		int k1 = 1;
		while (freq[k1] < freq[r1])
			k1++;

		k = Convert.ToInt32(freq[r] * 0.33);
		int r2 = 1;
		while (freq[r2] <= k)
			r2++;
		r2--;
		int k2 = 1;
		while (freq[k2] < freq[r2])
			k2++;

		if (n[k1] != s[k1])
		{
			t1 = (n[k1] / s[k1]) - 1.0;
			t2 = ((n[r] - n[k2]) / (s[r] - s[k2])) - 1.0;

			double part2 = 0.0;
			for (int t = 1; t <= r; t++)				
					part2 += (observedCount[t] * Math.Log((u * ((1.0 / t1) *
						Math.Pow((t1 / (1.0 + t1)), freq[t]))) +
						((1.0 - u) * ((1.0 / t2) *
						Math.Pow((t2 / (1.0 + t2)), freq[t])))));

			double tolerance = Math.Pow(10.0, -10.0);
			double part2Old = part2;
			double deltaPart2 = 0.0;
			int iteration = 0;
			do
			{
				double[] z = new double[r + 1];
				for (int t = 1; t <= r; t++)
					z[t] = (u * (1.0 / t1) * Math.Pow((t1 / (1.0 + t1)), freq[t])) /
						((u * (1.0 / t1) * Math.Pow((t1 / (1.0 + t1)), freq[t])) +
						((1.0 - u) * (1.0 / t2) * Math.Pow((t2 / (1.0 + t2)), freq[t])));

				u = 0.0;
				t1 = 0.0;
				t2 = 0.0;

				double t1Part1 = 0.0;
				double t1Part2 = 0.0;

				double t2Part1 = 0.0;
				double t2Part2 = 0.0;
				for (int t = 1; t <= r; t++)
				{
					u += (observedCount[t] * z[t]);

					t1Part1 += observedCount[t] * freq[t] * z[t];
					t2Part1 += observedCount[t] * freq[t] * (1.0 - z[t]);

					t1Part2 += observedCount[t] * z[t];
					t2Part2 += observedCount[t] * (1.0 - z[t]);
				}

				u *= (1.0 / s[r]);
				t1 = (t1Part1 / t1Part2) - 1.0;
				t2 = (t2Part1 / t2Part2) - 1.0;

				part2 = 0.0;
				for (int t = 1; t <= r; t++)
					part2 += (observedCount[t] * Math.Log((u * ((1.0 / t1) *
					Math.Pow((t1 / (1.0 + t1)), freq[t]))) +
					((1.0 - u) * ((1.0 / t2) * Math.Pow((t2 / (1.0 + t2)), freq[t])))));

				deltaPart2 = (part2 - part2Old);

				part2Old = part2;

				iteration++;
			}
			while (deltaPart2 > tolerance);

			if (Double.IsNaN(part2))
				return 0;
			else
				return 1;
		}
		else
			return 0;
	}
	
	//*********************************************************************
	//**                    Non Parametric Models                       ***
	//*********************************************************************
	public double NonParametricModels(StreamWriter swAnalysis, int fMin,
		int modelNum)
	{
		double[] GTEstimate = new double[obsMax + 1];
		double[] gammaSqRare = new double[obsMax + 1];
		double[] G = new double[obsMax + 1];
		double[] cvRare = new double[obsMax + 1];

		// need to account for datasets with no singletons
		double singletons = 0.0;
		if (freq[1] == 1)
			singletons = observedCount[1];

		//find fmin frequency
		int r = 1;
		while (freq[r] < fMin)
			r++;
		int freqMin = r;

		for (r = freqMin; r <= obsMax; r++)
		{	
			//Good Turing				
			GTEstimate[r] = s[r] / (1.0 - (singletons / n[r]));

			G[r] = 0.0;
			int rr = 1;
			for (int i = 1; i <= freq[r]; i++)
			{
				if (i == freq[rr])
				{
					G[r] += (i * (i - 1) * observedCount[rr]);
					rr++;
				}
			}

			gammaSqRare[r] = (GTEstimate[r] * G[r] /
				(n[r] * (n[r] - 1.0))) - 1.0;

			if (gammaSqRare[r] < 0.0)
			{
				gammaSqRare[r] = 0.0;
				cvRare[r] = 0.0;
			}
			else
				cvRare[r] = Math.Sqrt(gammaSqRare[r]);
		}

		GoodTuringModel(swAnalysis, freqMin, GTEstimate, singletons);

		Chao1Model(swAnalysis, freqMin, singletons);

		ACEModel(swAnalysis, freqMin, GTEstimate, G, gammaSqRare, cvRare,
		   modelNum, singletons);

		ACE1Model(swAnalysis, freqMin, GTEstimate, gammaSqRare, cvRare,
			G, modelNum, singletons);

		ChaoBungeModel(swAnalysis, freqMin, singletons);

		return cvRare[freqTau10]; 
	}

	

    //*********************************************************************
    //**           Calculate Standard Error for 4 Mixed Exp             ***
    //*********************************************************************
    public static double SEFourMixedExp(double t1, double t2, double t3,
        double t4, double t5, double t6, double t7, double sHatSubset,
        ref int SEFlag)
    {
        double a00 = -(-t7*t2*t4+t6*t2*t3+t6*t2+t7*t3+t7*t1*t2*t3-
            t6*t1*t4+t6*t1*t2-t5*t2*t4-t6*t3*t4+t2*t3*t4+
            t7*t2*t3-t5*t3*t4+t7*t1*t3+t1*t2*t4+t5*t1*t2-t7*t1*t4+
            t1*t3*t4+t5*t1*t3-t5*t2*t3*t4+t5*t1-t6*t1*t3*t4+t4+
            t5*t1*t2*t3-t7*t1*t2*t4+t6*t1*t2*t3+t3*t4+t2*t4-t5*t4+
            t1*t4-t6*t4+t1*t2*t3*t4-t7*t4)/(-1-t7*t2*t4+t6*t2*t3-t2-
            t1+t6*t2+t7*t3+t7*t1*t2*t3-t6*t1*t4+t6*t1*t2-t5*t2*t4-
            t6*t3*t4+t7*t2*t3-t5*t3*t4+t7*t1*t3+t5*t1*t2-t7*t1*t4+
            t5*t1*t3-t1*t2*t3-t5*t2*t3*t4+t5*t1-t3-t6*t1*t3*t4+
            t5*t1*t2*t3-t7*t1*t2*t4+t6*t1*t2*t3-t2*t3-t5*t4-t1*t3-
            t6*t4-t1*t2-t7*t4);

        double[] a0 = new double[8];
        a0[1] = -t5*(1+t4)*(1+t3)*(1+t2)/(1+t1)/(-1-t7*t2*t4+t6*t2*t3-
            t2-t1+t6*t2+t7*t3+t7*t1*t2*t3-t6*t1*t4+t6*t1*t2-
            t5*t2*t4-t6*t3*t4+t7*t2*t3-t5*t3*t4+t7*t1*t3+t5*t1*t2-
            t7*t1*t4+t5*t1*t3-t1*t2*t3-t5*t2*t3*t4+t5*t1-t3-
            t6*t1*t3*t4+t5*t1*t2*t3-t7*t1*t2*t4+t6*t1*t2*t3-t2*t3-
            t5*t4-t1*t3-t6*t4-t1*t2-t7*t4);

        a0[2]=-t6*(t1*t4+t3*t4+t1*t3+t1*t3*t4+1+t4+t3+t1)/(1+t2)/
            (-1-t7*t2*t4+t6*t2*t3-t2-t1+t6*t2+t7*t3+t7*t1*t2*t3-
            t6*t1*t4+t6*t1*t2-t5*t2*t4-t6*t3*t4+t7*t2*t3-t5*t3*t4+
            t7*t1*t3+t5*t1*t2-t7*t1*t4+t5*t1*t3-t1*t2*t3-t5*t2*t3*t4+
            t5*t1-t3-t6*t1*t3*t4+t5*t1*t2*t3-t7*t1*t2*t4+t6*t1*t2*t3-
            t2*t3-t5*t4-t1*t3-t6*t4-t1*t2-t7*t4);

        a0[3]=-(1+t2)*(1+t4)*t7*(1+t1)/(1+t3)/(-1-t7*t2*t4+t6*t2*t3-t2-
            t1+t6*t2+t7*t3+t7*t1*t2*t3-t6*t1*t4+t6*t1*t2-t5*t2*t4-
            t6*t3*t4+t7*t2*t3-t5*t3*t4+t7*t1*t3+t5*t1*t2-t7*t1*t4+
            t5*t1*t3-t1*t2*t3-t5*t2*t3*t4+t5*t1-t3-t6*t1*t3*t4+
            t5*t1*t2*t3-t7*t1*t2*t4+t6*t1*t2*t3-t2*t3-t5*t4-t1*t3-
            t6*t4-t1*t2-t7*t4);

        a0[4]=(1+t2)*(t7*t1+t6*t3-1+t6+t7*t1*t3+t6*t1+t7*t3+t5*t1+
            t5*t3-t1*t3+t5*t1*t3+t6*t1*t3-t3+t5-t1+t7)/(1+t4)/
            (-1-t7*t2*t4+t6*t2*t3-t2-t1+t6*t2+t7*t3+t7*t1*t2*t3-
            t6*t1*t4+t6*t1*t2-t5*t2*t4-t6*t3*t4+t7*t2*t3-t5*t3*t4+
            t7*t1*t3+t5*t1*t2-t7*t1*t4+t5*t1*t3-t1*t2*t3-t5*t2*t3*t4+
            t5*t1-t3-t6*t1*t3*t4+t5*t1*t2*t3-t7*t1*t2*t4+t6*t1*t2*t3-
            t2*t3-t5*t4-t1*t3-t6*t4-t1*t2-t7*t4);

        a0[5]=-1/(-1-t7*t2*t4+t6*t2*t3-t2-t1+t6*t2+t7*t3+t7*t1*t2*t3-
            t6*t1*t4+t6*t1*t2-t5*t2*t4-t6*t3*t4+t7*t2*t3-t5*t3*t4+
            t7*t1*t3+t5*t1*t2-t7*t1*t4+t5*t1*t3-t1*t2*t3-t5*t2*t3*t4+
            t5*t1-t3-t6*t1*t3*t4+t5*t1*t2*t3-t7*t1*t2*t4+t6*t1*t2*t3-
            t2*t3-t5*t4-t1*t3-t6*t4-t1*t2-t7*t4)*(-t2*t4-t3*t4+t1*t2+
            t1*t3-t2*t3*t4+t1+t1*t2*t3-t4);

        a0[6]=-1/(-1-t7*t2*t4+t6*t2*t3-t2-t1+t6*t2+t7*t3+t7*t1*t2*t3-
            t6*t1*t4+t6*t1*t2-t5*t2*t4-t6*t3*t4+t7*t2*t3-t5*t3*t4+
            t7*t1*t3+t5*t1*t2-t7*t1*t4+t5*t1*t3-t1*t2*t3-t5*t2*t3*t4+
            t5*t1-t3-t6*t1*t3*t4+t5*t1*t2*t3-t7*t1*t2*t4+t6*t1*t2*t3-
            t2*t3-t5*t4-t1*t3-t6*t4-t1*t2-t7*t4)*(t2*t3+t2-t1*t4+t1*t2-
            t3*t4-t1*t3*t4+t1*t2*t3-t4);

        a0[7]=-1/(-1-t7*t2*t4+t6*t2*t3-t2-t1+t6*t2+t7*t3+t7*t1*t2*t3-
            t6*t1*t4+t6*t1*t2-t5*t2*t4-t6*t3*t4+t7*t2*t3-t5*t3*t4+
            t7*t1*t3+t5*t1*t2-t7*t1*t4+t5*t1*t3-t1*t2*t3-t5*t2*t3*t4+
            t5*t1-t3-t6*t1*t3*t4+t5*t1*t2*t3-t7*t1*t2*t4+t6*t1*t2*t3-
            t2*t3-t5*t4-t1*t3-t6*t4-t1*t2-t7*t4)*(-t2*t4+t3+t1*t2*t3+
            t2*t3+t1*t3-t1*t4-t1*t2*t4-t4);

        double[,] A = new double[8, 8];

        double SE = 0.0;
        int iterFlag = 1;

        SEFourMixedExp1.SEFourMixedExpA1(t1, t2, t3, t4, t5, t6, t7, A, ref iterFlag);
        if (iterFlag == 0)
            return SE;

        SEFourMixedExp2.SEFourMixedExpA2(t1, t2, t3, t4, t5, t6, t7, A, ref iterFlag);
        if (iterFlag == 0)
            return SE;

        SEFourMixedExp3.SEFourMixedExpA3(t1, t2, t3, t4, t5, t6, t7, A, ref iterFlag);
        if (iterFlag == 0)
            return SE;

        SEFourMixedExp4.SEFourMixedExpA44(t1, t2, t3, t4, t5, t6, t7, A, ref iterFlag);
        if (iterFlag == 0)
            return SE;

        SEFourMixedExp5.SEFourMixedExpA45_47(t1, t2, t3, t4, t5, t6, t7, A, ref iterFlag);
        if (iterFlag == 0)
            return SE;

        SEFourMixedExpA5_A7(t1, t2, t3, t4, t5, t6, t7, A, ref iterFlag);
        if (iterFlag == 0)
            return SE;

        SE = MatrixInversion(7, sHatSubset, a00, a0, A, ref SEFlag);

        return SE;
    }
	

    //*********************************************************************
	//**           Calculate A5-A7 for SE 4 Mixed Exp                   ***
	//*********************************************************************
    public static void SEFourMixedExpA5_A7(double t1, double t2, double t3,
        double t4, double t5, double t6, double t7, double[,] A,
        ref int iterFlag)
    {
        double test = 100.0;
        int k = 0;

        //A55
        do
        {
            double t1P = Math.Pow((t1/(1+t1)),k);
            double t2P = Math.Pow((t2/(1+t2)),k);
            double t3P = Math.Pow((t3/(1+t3)),k);
            double t4P = Math.Pow((t4/(1+t4)),k);

            double A55 = 1/(t7*t3P+t6*t2P+t5*t1P+t4P-t4P*t5*t1*t2*t3+t5*t1P*t2+t6*t2P*t4+t6*t2P*t3+t6*t2P*t1
				+t7*t3P*t4+t7*t3P*t2+t7*t3P*t1-t4P*t6*t2-t4P*t7*t3-t4P*t5*t1+t4P*t2*t3-t4P*t5*t3-t4P
				*t5*t2+t4P*t1*t3-t4P*t6*t3-t4P*t6*t1+t4P*t1*t2-t4P*t7*t2-t4P*t7*t1+t5*t1P*t4+t5*t1P*t3
				+t4P*t3-t4P*t5-t4P*t6-t4P*t7+t4P*t2+t4P*t1+t5*t1P*t2*t4+t5*t1P*t3*t4+t5*t1P*t2*t3+t6
				*t2P*t1*t4+t6*t2P*t3*t4+t6*t2P*t1*t3+t7*t3P*t2*t4+t7*t3P*t1*t2+t7*t3P*t1*t4-t4P*t7*t1*t2-t4P
				*t6*t2*t3-t4P*t6*t1*t2-t4P*t7*t2*t3-t4P*t7*t1*t3-t4P*t6*t1*t3-t4P*t5*t1*t2-t4P*t5*t1*t3-t4P
				*t5*t2*t3+t4P*t1*t2*t3-t4P*t6*t1*t2*t3+t5*t1P*t2*t3*t4+t6*t2P*t1*t3*t4+t7*t3P*t1*t2*t4-t4P
				*t7*t1*t2*t3)/(1+t1)*(1+t2)*(1+t3)/(1+t4)*Math.Pow(t1P+t1P*t4-t4P-t4P*t1,2);

            if (k > 0)
                test = Math.Abs(A55 / A[5, 5]);

            A[5, 5] += A55;
            k++;

        } while (test > Criteria && k < maxIter);

        if (k == maxIter)
        {
            iterFlag = 0;
            return;
        }

       //A56
		test = 100.0;
		k = 0;
		do
		{
            double t1P = Math.Pow((t1/(1+t1)),k);
            double t2P = Math.Pow((t2/(1+t2)),k);
            double t3P = Math.Pow((t3/(1+t3)),k);
            double t4P = Math.Pow((t4/(1+t4)),k); 
            
            double t42kP = Math.Pow((t4/(1+t4)),(2*k));
            
            double A56 = -(1+t3)*(-t2P*t1P-2*t2P*t1P*t4-t2P*t1P*t4*t4+t2P*t4P+t2P*t4P*t1+t2P*t4P*t4+t2P
				*t4P*t1*t4+t4P*t1P+t4P*t1P*t2+t4P*t1P*t4+t4P*t1P*t2*t4-t42kP-t42kP*t1-t42kP*t2-t42kP
				*t1*t2)/(t7*t3P+t6*t2P+t5*t1P+t4P-t4P*t5*t1*t2*t3+t5*t1P*t2+t6*t2P*t4+t6*t2P*t3+t6*t2P
				*t1+t7*t3P*t4+t7*t3P*t2+t7*t3P*t1-t4P*t6*t2-t4P*t7*t3-t4P*t5*t1+t4P*t2*t3-t4P*t5*t3-t4P
				*t5*t2+t4P*t1*t3-t4P*t6*t3-t4P*t6*t1+t4P*t1*t2-t4P*t7*t2-t4P*t7*t1+t5*t1P*t4+t5*t1P*t3
				+t4P*t3-t4P*t5-t4P*t6-t4P*t7+t4P*t2+t4P*t1+t5*t1P*t2*t4+t5*t1P*t3*t4+t5*t1P*t2*t3+t6
				*t2P*t1*t4+t6*t2P*t3*t4+t6*t2P*t1*t3+t7*t3P*t2*t4+t7*t3P*t1*t2+t7*t3P*t1*t4-t4P*t7*t1*t2-t4P
				*t6*t2*t3-t4P*t6*t1*t2-t4P*t7*t2*t3-t4P*t7*t1*t3-t4P*t6*t1*t3-t4P*t5*t1*t2-t4P*t5*t1*t3-t4P
				*t5*t2*t3+t4P*t1*t2*t3-t4P*t6*t1*t2*t3+t5*t1P*t2*t3*t4+t6*t2P*t1*t3*t4+t7*t3P*t1*t2*t4-t4P
				*t7*t1*t2*t3)/(1+t4);

            if (k > 0)
                test = Math.Abs(A56 / A[5, 6]);

            A[5, 6] += A56;
            k++;

        } while (test > Criteria && k < maxIter);


        if (k == maxIter)
        {
            iterFlag = 0;
            return;
        }
        
       //A57
		test = 100.0;
		k = 0;
		do
		{
            double t1P = Math.Pow((t1/(1+t1)),k);
            double t2P = Math.Pow((t2/(1+t2)),k);
            double t3P = Math.Pow((t3/(1+t3)),k);
            double t4P = Math.Pow((t4/(1+t4)),k);

            double t42kP = Math.Pow((t4/(1+t4)),(2*k));
            
            double A57 = -(1+t2)*(-t3P*t1P-2*t3P*t1P*t4-t3P*t1P*t4*t4+t3P*t4P+t3P*t4P*t1+t3P*t4P*t4+t3P
				*t4P*t1*t4+t4P*t1P+t4P*t1P*t3+t4P*t1P*t4+t4P*t1P*t3*t4-t42kP-t42kP*t3-t42kP*t1-t42kP
				*t1*t3)/(t7*t3P+t6*t2P+t5*t1P+t4P-t4P*t5*t1*t2*t3+t5*t1P*t2+t6*t2P*t4+t6*t2P*t3+t6*t2P
				*t1+t7*t3P*t4+t7*t3P*t2+t7*t3P*t1-t4P*t6*t2-t4P*t7*t3-t4P*t5*t1+t4P*t2*t3-t4P*t5*t3-t4P
				*t5*t2+t4P*t1*t3-t4P*t6*t3-t4P*t6*t1+t4P*t1*t2-t4P*t7*t2-t4P*t7*t1+t5*t1P*t4+t5*t1P*t3
				+t4P*t3-t4P*t5-t4P*t6-t4P*t7+t4P*t2+t4P*t1+t5*t1P*t2*t4+t5*t1P*t3*t4+t5*t1P*t2*t3+t6
				*t2P*t1*t4+t6*t2P*t3*t4+t6*t2P*t1*t3+t7*t3P*t2*t4+t7*t3P*t1*t2+t7*t3P*t1*t4-t4P*t7*t1*t2-t4P
				*t6*t2*t3-t4P*t6*t1*t2-t4P*t7*t2*t3-t4P*t7*t1*t3-t4P*t6*t1*t3-t4P*t5*t1*t2-t4P*t5*t1*t3-t4P
				*t5*t2*t3+t4P*t1*t2*t3-t4P*t6*t1*t2*t3+t5*t1P*t2*t3*t4+t6*t2P*t1*t3*t4+t7*t3P*t1*t2*t4-t4P
				*t7*t1*t2*t3)/(1+t4);

            if (k > 0)
                test = Math.Abs(A57 / A[5, 7]);

            A[5, 7] += A57;
            k++;

        } while (test > Criteria && k < maxIter);


        if (k == maxIter)
        {
            iterFlag = 0;
            return;
        }

        //A66
		test = 100.0;
		k = 0;
		do
		{
            double t1P = Math.Pow((t1/(1+t1)),k);
            double t2P = Math.Pow((t2/(1+t2)),k);
            double t3P = Math.Pow((t3/(1+t3)),k);
            double t4P = Math.Pow((t4/(1+t4)),k); 
            
            double A66 = 1/(t7*t3P+t6*t2P+t5*t1P+t4P-t4P*t5*t1*t2*t3+t5*t1P*t2+t6*t2P*t4+t6*t2P*t3+t6*t2P*t1
				+t7*t3P*t4+t7*t3P*t2+t7*t3P*t1-t4P*t6*t2-t4P*t7*t3-t4P*t5*t1+t4P*t2*t3-t4P*t5*t3-t4P
				*t5*t2+t4P*t1*t3-t4P*t6*t3-t4P*t6*t1+t4P*t1*t2-t4P*t7*t2-t4P*t7*t1+t5*t1P*t4+t5*t1P*t3
				+t4P*t3-t4P*t5-t4P*t6-t4P*t7+t4P*t2+t4P*t1+t5*t1P*t2*t4+t5*t1P*t3*t4+t5*t1P*t2*t3+t6
				*t2P*t1*t4+t6*t2P*t3*t4+t6*t2P*t1*t3+t7*t3P*t2*t4+t7*t3P*t1*t2+t7*t3P*t1*t4-t4P*t7*t1*t2-t4P
				*t6*t2*t3-t4P*t6*t1*t2-t4P*t7*t2*t3-t4P*t7*t1*t3-t4P*t6*t1*t3-t4P*t5*t1*t2-t4P*t5*t1*t3-t4P
				*t5*t2*t3+t4P*t1*t2*t3-t4P*t6*t1*t2*t3+t5*t1P*t2*t3*t4+t6*t2P*t1*t3*t4+t7*t3P*t1*t2*t4-t4P
				*t7*t1*t2*t3)*(1+t1)/(1+t2)*(1+t3)/(1+t4)*Math.Pow(-t2P-t2P*t4+t4P+t4P*t2,2);

            if (k > 0)
                test = Math.Abs(A66 / A[6, 6]);

            A[6, 6] += A66;
            k++;

        } while (test > Criteria && k < maxIter);

        if (k == maxIter)
        {
            iterFlag = 0;
            return;
        }

        //A67
		test = 100.0;
		k = 0;
		do
		{
            double t1P = Math.Pow((t1/(1+t1)),k);
            double t2P = Math.Pow((t2/(1+t2)),k);
            double t3P = Math.Pow((t3/(1+t3)),k);
            double t4P = Math.Pow((t4/(1+t4)),k); 
            
            double t42kP = Math.Pow((t4/(1+t4)),(2*k));
            
            double A67 = -(1+t1)*(-t3P*t2P-2*t3P*t2P*t4-t3P*t2P*t4*t4+t3P*t4P+t3P*t4P*t2+t3P*t4P*t4+t3P
				*t4P*t2*t4+t2P*t4P+t2P*t4P*t3+t2P*t4P*t4+t2P*t4P*t3*t4-t42kP-t42kP*t3-t42kP*t2-t42kP
				*t2*t3)/(t7*t3P+t6*t2P+t5*t1P+t4P-t4P*t5*t1*t2*t3+t5*t1P*t2+t6*t2P*t4+t6*t2P*t3+t6*t2P
				*t1+t7*t3P*t4+t7*t3P*t2+t7*t3P*t1-t4P*t6*t2-t4P*t7*t3-t4P*t5*t1+t4P*t2*t3-t4P*t5*t3-t4P
				*t5*t2+t4P*t1*t3-t4P*t6*t3-t4P*t6*t1+t4P*t1*t2-t4P*t7*t2-t4P*t7*t1+t5*t1P*t4+t5*t1P*t3
				+t4P*t3-t4P*t5-t4P*t6-t4P*t7+t4P*t2+t4P*t1+t5*t1P*t2*t4+t5*t1P*t3*t4+t5*t1P*t2*t3+t6
				*t2P*t1*t4+t6*t2P*t3*t4+t6*t2P*t1*t3+t7*t3P*t2*t4+t7*t3P*t1*t2+t7*t3P*t1*t4-t4P*t7*t1*t2-t4P
				*t6*t2*t3-t4P*t6*t1*t2-t4P*t7*t2*t3-t4P*t7*t1*t3-t4P*t6*t1*t3-t4P*t5*t1*t2-t4P*t5*t1*t3-t4P
				*t5*t2*t3+t4P*t1*t2*t3-t4P*t6*t1*t2*t3+t5*t1P*t2*t3*t4+t6*t2P*t1*t3*t4+t7*t3P*t1*t2*t4-t4P
				*t7*t1*t2*t3)/(1+t4);

            if (k > 0)
                test = Math.Abs(A67 / A[6, 7]);

            A[6, 7] += A67;
            k++;

        } while (test > Criteria && k < maxIter);

        if (k == maxIter)
        {
            iterFlag = 0;
            return;
        }

         //A77
		test = 100.0;
		k = 0;
		do
		{
            double t1P = Math.Pow((t1/(1+t1)),k);
            double t2P = Math.Pow((t2/(1+t2)),k);
            double t3P = Math.Pow((t3/(1+t3)),k);
            double t4P = Math.Pow((t4/(1+t4)),k); 
            
            double A77 = 1/(t7*t3P+t6*t2P+t5*t1P+t4P-t4P*t5*t1*t2*t3+t5*t1P*t2+t6*t2P*t4+t6*t2P*t3+t6*t2P*t1
				+t7*t3P*t4+t7*t3P*t2+t7*t3P*t1-t4P*t6*t2-t4P*t7*t3-t4P*t5*t1+t4P*t2*t3-t4P*t5*t3-t4P
				*t5*t2+t4P*t1*t3-t4P*t6*t3-t4P*t6*t1+t4P*t1*t2-t4P*t7*t2-t4P*t7*t1+t5*t1P*t4+t5*t1P*t3
				+t4P*t3-t4P*t5-t4P*t6-t4P*t7+t4P*t2+t4P*t1+t5*t1P*t2*t4+t5*t1P*t3*t4+t5*t1P*t2*t3+t6
				*t2P*t1*t4+t6*t2P*t3*t4+t6*t2P*t1*t3+t7*t3P*t2*t4+t7*t3P*t1*t2+t7*t3P*t1*t4-t4P*t7*t1*t2-t4P
				*t6*t2*t3-t4P*t6*t1*t2-t4P*t7*t2*t3-t4P*t7*t1*t3-t4P*t6*t1*t3-t4P*t5*t1*t2-t4P*t5*t1*t3-t4P
				*t5*t2*t3+t4P*t1*t2*t3-t4P*t6*t1*t2*t3+t5*t1P*t2*t3*t4+t6*t2P*t1*t3*t4+t7*t3P*t1*t2*t4-t4P
				*t7*t1*t2*t3)*(1+t1)*(1+t2)/(1+t3)/(1+t4)*Math.Pow(-t3P-t3P*t4+t4P+t4P*t3,2);

            if (k > 0)
                test = Math.Abs(A77 / A[7, 7]);

            A[7, 7] += A77;
            k++;

        } while (test > Criteria && k < maxIter);

        if (k == maxIter)
        {
            iterFlag = 0;
            return;
        }

    }

    //*********************************************************************
    //**           Calculate Standard Error for 3 Mixed Exp             ***
    //*********************************************************************
    public static double SEThreeMixedExp(double t1, double t2, double t3,
        double t4, double t5, double sHatSubset, ref int SEFlag)
    {
        double t23P = Math.Pow(t2, 3);
        double t33P = Math.Pow(t3, 3);

        double t13P = Math.Pow(t1, 3);
        double t1bP = Math.Pow(t1, (-2));
        double t1aP = Math.Pow((1 + t1), (-3));
        double t2aP = Math.Pow((1 + t2), (-3));
        double t2bP = Math.Pow(t2, (-2));

        double t3aP = Math.Pow((1 + t3), (-3));
        double t3bP = Math.Pow(t3, (-2));

        double a00 = -((-t4 * t3) - (t4 * t2 * t3) - (t5 * t3) - (t5 * t1 * t3) + (t4 * t1) +
            (t4 * t1 * t2) + (t5 * t2) + (t5 * t1 * t2) + t3 + (t2 * t3) + (t1 * t3) + (t1 * t2 * t3)) /
            ((-t4 * t3) - (t4 * t2 * t3) - (t5 * t3) - (t5 * t1 * t3) - 1 - t2 - t1 - (t1 * t2) +
            (t4 * t1) + (t4 * t1 * t2) + (t5 * t2) + (t5 * t1 * t2));

        double[] a0 = new double[6];
        a0[1] = -t4 * (1 + t2) * (1 + t3) / (1 + t1) / (-t4 * t3 - t4 * t2 * t3 - t5 * t3 - t5 * t1 *
            t3 - 1 - t2 - t1 - t1 * t2 + t4 * t1 + t4 * t1 * t2 + t5 * t2 + t5 * t1 * t2);

        a0[2] = -t5 * (1 + t3 + t1 + t1 * t3) / (1 + t2) / (-t4 * t3 - t4 * t2 * t3 - t5 * t3 - t5 *
            t1 * t3 - 1 - t2 - t1 - t1 * t2 + t4 * t1 + t4 * t1 * t2 + t5 * t2 + t5 * t1 * t2);

        a0[3] = (-1 - t1 + t4 + t4 * t1 + t5 + t5 * t1) * (1 + t2) / (1 + t3) /
            (-t4 * t3 - t4 * t2 * t3 - t5 * t3 - t5 * t1 * t3 - 1 - t2 - t1 - t1 * t2 + t4 * t1 + t4 *
            t1 * t2 + t5 * t2 + t5 * t1 * t2);

        a0[4] = -1 / (-t4 * t3 - t4 * t2 * t3 - t5 * t3 - t5 * t1 * t3 - 1 - t2 - t1 - t1 * t2 + t4 *
            t1 + t4 * t1 * t2 + t5 * t2 + t5 * t1 * t2) * (-t3 - t2 * t3 + t1 + t1 * t2);

        a0[5] = -1 / (-t4 * t3 - t4 * t2 * t3 - t5 * t3 - t5 * t1 * t3 - 1 - t2 - t1 - t1 * t2 + t4 *
            t1 + t4 * t1 * t2 + t5 * t2 + t5 * t1 * t2) * (-t3 - t1 * t3 + t2 + t1 * t2);

        double[,] A = new double[6, 6];

        double SE = 0.0;

        //A11
        double test = 100.0;
        int k = 0;
        do
        {
            double t1P = Math.Pow((t1 / (1 + t1)), k);
            double t2P = Math.Pow((t2 / (1 + t2)), k);
            double t3P = Math.Pow((t3 / (1 + t3)), k);

            double A11 = -t4 * t1P * (2 * t4 * t1P * k * t1 * t2 - (t1 * t1) * t4 *
                t1P * t2 * t3 - 2 * (t1 * t1) * t5 * t2P * t3 - 2 * t13P * t5 *
                t2P * t3 + 2 * (t1 * t1) * t3P * t4 * t2 + 2 * t13P *
                t3P * t4 * t2 + 2 * (t1 * t1) * t3P * t5 * t2 + 2 * t13P *
                t3P * t5 * t2 + 5 * k * t1 * t5 * t2P + 4 * k * (t1 * t1) * t5 *
                t2P + 5 * k * t1 * t3P * t2 - 4 * k * (t1 * t1) *
                t3P * t4 - 5 * k * t1 * t3P * t5 - 4 * k * (t1 * t1) *
                t3P * t5 - k * k * t5 * t2P * t3 - k * k * t5 * t2P * t1 - k * k *
                t3P * t1 * t2 + t3P * k * t2 - 2 * t13P *
                t3P + 2 * k * t1 * t4 * t1P * t3 + 2 * k * t1 * t4 *
                t1P * t2 * t3 + 5 * k * t1 * t5 * t2P * t3 + 4 * k * (t1 * t1) * t5 *
                t2P * t3 - 5 * k * t1 * t3P * t4 * t2 + k * k *
                t3P * t4 * t2 + k * k * t3P * t4 * t1 + k * k *
                t3P * t5 * t2 + k * k * t3P * t5 * t1 + k * t4 *
                t1P * t3 + k * t5 * t2P * t3 - k * t3P * t4 * t2 - k *
                t3P * t5 * t2 + 2 * t4 * t1P * k * t1 + t3P * k + 5 *
                t3P * k * t1 - 4 * k * (t1 * t1) * t3P * t4 * t2 - 5 * k * t1 *
                t3P * t5 * t2 - 4 * k * (t1 * t1) * t3P * t5 * t2 - k * k * t5 *
                t2P * t1 * t3 + k * k * t3P * t4 * t1 * t2 + k * k *
                t3P * t5 * t1 * t2 + k * t4 * t1P * t2 * t3 - 2 * t13P *
                t3P * t2 + 2 * (t1 * t1) * t3P * t4 + 2 * t13P *
                t3P * t4 + 2 * (t1 * t1) * t3P * t5 + 2 * t13P *
                t3P * t5 + 4 * k * (t1 * t1) * t3P - k * k * t5 * t2P + t4 *
                t1P * k - 2 * (t1 * t1) * t3P * t2 - 2 * t13P * t5 *
                t2P - 2 * (t1 * t1) * t5 * t2P - k * k * t3P * t2 - k * k *
                t3P * t1 + k * k * t3P * t4 + k * k * t3P * t5 + k * t5 *
                t2P - k * t3P * t4 - k * t3P * t5 - t4 *
                t1P * (t1 * t1) - t2 * t4 * t1P * (t1 * t1) + t4 *
                t1P * k * t2 - 5 * k * t1 * t3P * t4 + 4 * k * (t1 * t1) *
                t3P * t2 - (t1 * t1) * t4 * t1P * t3 - k * k *
                t3P - 2 * (t1 * t1) * t3P) * t1aP / (-t4 *
                t1P - t4 * t1P * t3 - t4 * t1P * t2 - t4 *
                t1P * t2 * t3 - t5 * t2P * t3 - t5 * t2P * t1 * t3 - t5 *
                t2P - t5 * t2P * t1 - t3P - t3P * t2 -
                t3P * t1 - t3P * t1 * t2 + t3P * t4 +
                t3P * t4 * t2 + t3P * t4 * t1 + t3P * t4 * t1 * t2 +
                t3P * t5 + t3P * t5 * t2 + t3P * t5 * t1 +
                t3P * t5 * t1 * t2) * t1bP;

            if (k > 0)
                test = Math.Abs(A11 / A[1, 1]);

            A[1, 1] += A11;
            k++;

        } while (test > Criteria && k < maxIter);

        if (k == maxIter)
            return SE;

        //A12
        test = 100.0;
        k = 0;
        do
        {
            double t1P = Math.Pow((t1 / (1 + t1)), k);
            double t2P = Math.Pow((t2 / (1 + t2)), k);
            double t3P = Math.Pow((t3 / (1 + t3)), k);

            double A12 = -(1 + t3) * t1P * t4 * (-t1 + k) * t5 *
                t2P * (-t2 + k) / t2 / (1 + t2) / t1 / (1 + t1) / (-t4 * t1P - t4 *
                t1P * t3 - t4 * t1P * t2 - t4 * t1P * t2 * t3 - t5 *
                t2P - t5 * t2P * t3 - t5 * t2P * t1 - t5 *
                t2P * t1 * t3 - t3P - t3P * t2 -
                t3P * t1 - t3P * t1 * t2 + t3P * t4 +
                t3P * t4 * t2 + t3P * t4 * t1 + t3P * t4 * t1 * t2 +
                t3P * t5 + t3P * t5 * t2 + t3P * t5 * t1 +
                t3P * t5 * t1 * t2);

            if (k > 0)
                test = Math.Abs(A12 / A[1, 2]);

            A[1, 2] += A12;
            k++;

        } while (test > Criteria && k < maxIter);

        if (k == maxIter)
            return SE;

        //A13
        test = 100.0;
        k = 0;
        do
        {
            double t1P = Math.Pow((t1 / (1 + t1)), k);
            double t2P = Math.Pow((t2 / (1 + t2)), k);
            double t3P = Math.Pow((t3 / (1 + t3)), k);

            double A13 = (1 + t2) * t1P * t4 * (-t1 + k) * (-1 + t4 + t5) *
                t3P * (-t3 + k) / t3 / (1 + t3) / t1 / (1 + t1) / (-t4 * t1P - t4 *
                t1P * t3 - t4 * t1P * t2 - t4 * t1P * t2 * t3 - t5 *
                t2P - t5 * t2P * t3 - t5 * t2P * t1 - t5 *
                t2P * t1 * t3 - t3P - t3P * t2 -
                t3P * t1 - t3P * t1 * t2 + t3P * t4 +
                t3P * t4 * t2 + t3P * t4 * t1 + t3P * t4 * t1 * t2 +
                t3P * t5 + t3P * t5 * t2 + t3P * t5 * t1 +
                t3P * t5 * t1 * t2);

            if (k > 0)
                test = Math.Abs(A13 / A[1, 3]);

            A[1, 3] += A13;
            k++;

        } while (test > Criteria && k < maxIter);

        if (k == maxIter)
            return SE;

        //A14
        test = 100.0;
        k = 0;
        do
        {
            double t1P = Math.Pow((t1 / (1 + t1)), k);
            double t2P = Math.Pow((t2 / (1 + t2)), k);
            double t3P = Math.Pow((t3 / (1 + t3)), k);

            double A14 = -t1P * (-t1 + k) * (t5 * t2P * t3 + t5 * t2P +
                t3P * t2 - t3P * t5 * t2 + t3P -
                t3P * t5) / (t4 * t1P + t4 * t1P * t3 + t4 *
                t1P * t2 + t4 * t1P * t2 * t3 + t5 * t2P + t5 *
                t2P * t3 + t5 * t2P * t1 + t5 * t2P * t1 * t3 +
                t3P + t3P * t2 + t3P * t1 + t3P * t1 * t2 -
                t3P * t4 - t3P * t4 * t2 - t3P * t4 * t1 -
                t3P * t4 * t1 * t2 - t3P * t5 - t3P * t5 * t2 -
                t3P * t5 * t1 - t3P * t5 * t1 * t2) / t1 / (1 + t1);

            if (k > 0)
                test = Math.Abs(A14 / A[1, 4]);

            A[1, 4] += A14;
            k++;

        } while (test > Criteria && k < maxIter);

        if (k == maxIter)
            return SE;

        //A15
        test = 100.0;
        k = 0;
        do
        {
            double t1P = Math.Pow((t1 / (1 + t1)), k);
            double t2P = Math.Pow((t2 / (1 + t2)), k);
            double t3P = Math.Pow((t3 / (1 + t3)), k);

            double A15 = t1P * t4 * (-t1 + k) * (t2P + t2P * t3 -
                t3P - t3P * t2) / t1 / (1 + t1) / (t4 * t1P + t4 *
                t1P * t3 + t4 * t1P * t2 + t4 * t1P * t2 * t3 + t5 *
                t2P + t5 * t2P * t3 + t5 * t2P * t1 + t5 *
                t2P * t1 * t3 + t3P + t3P * t2 +
                t3P * t1 + t3P * t1 * t2 - t3P * t4 -
                t3P * t4 * t2 - t3P * t4 * t1 - t3P * t4 * t1 * t2 -
                t3P * t5 - t3P * t5 * t2 - t3P * t5 * t1 -
                t3P * t5 * t1 * t2);

            if (k > 0)
                test = Math.Abs(A15 / A[1, 5]);

            A[1, 5] += A15;
            k++;

        } while (test > Criteria && k < maxIter);

        if (k == maxIter)
            return SE;

        //A22
        test = 100.0;
        k = 0;
        do
        {
            double t1P = Math.Pow((t1 / (1 + t1)), k);
            double t2P = Math.Pow((t2 / (1 + t2)), k);
            double t3P = Math.Pow((t3 / (1 + t3)), k);

            double A22 = -t5 * t2P * (-k * k * t3P * t5 + k *
                t3P * t4 + k * k * t4 * t1P + 2 * (t2 * t2) * t4 * t1P + 2 *
                t23P * t4 * t1P + 2 * (t2 * t2) * t3P * t1 + 2 * t23P *
                t3P * t1 - 4 * k * (t2 * t2) * t3P - 2 *
                t3P * t5 * (t2 * t2) - 2 * t5 * t23P * t3P - 2 * t4 * t23P *
                t3P - 2 * t3P * (t2 * t2) * t4 + 5 * k * t1 *
                t3P * t4 * t2 + 2 * (t2 * t2) * t3P + 2 * t23P *
                t3P - k * t5 * t2P - k * k * t3P * t4 + k * k *
                t3P * t1 - k * t1 * t5 * t2P - 5 * k * t1 *
                t3P * t2 + k * t1 * t3P * t4 + k * t1 * t3P * t5 + k * k *
                t3P * t1 * t2 - k * k * t3P * t4 * t2 - k * k *
                t3P * t4 * t1 - k * k * t3P * t5 * t2 - k * k *
                t3P * t5 * t1 - k * t4 * t1P * t3 - 5 * k * t4 *
                t1P * t2 - k * t5 * t2P * t3 + 5 * k *
                t3P * t4 * t2 + 5 * k * t3P * t5 * t2 - k * t1 * t5 *
                t2P * t3 + 5 * k * t1 * t3P * t5 * t2 - k * k *
                t3P * t4 * t1 * t2 - k * k * t3P * t5 * t1 * t2 - 5 * k * t4 *
                t1P * t2 * t3 + k * k * t3P * t2 - 2 * t5 *
                t2P * k * t1 * t2 - 2 * k * t2 * t5 * t2P * t1 * t3 + k * k * t4 *
                t1P * t3 + k * k * t4 * t1P * t2 + t1 * t5 *
                t2P * (t2 * t2) + 4 * t3P * k * t4 * (t2 * t2) - 2 * t5 *
                t2P * k * t2 - 2 * t3P * t5 * t1 * t23P - 2 *
                t3P * t5 * t1 * (t2 * t2) + 4 * t5 * k * (t2 * t2) * t3P + 2 * (t2 * t2) * t4 *
                t1P * t3 + 2 * t23P * t4 * t1P * t3 + (t2 * t2) * t5 *
                t2P * t3 - 4 * k * (t2 * t2) * t4 * t1P - 4 * k * (t2 * t2) *
                t3P * t1 - 2 * t3P * t1 * t23P * t4 - 2 *
                t3P * t1 * (t2 * t2) * t4 + (t2 * t2) * t5 * t2P * t1 * t3 - 4 * k * (t2 * t2) * t4 *
                t1P * t3 + 4 * t3P * k * t4 * (t2 * t2) * t1 + 4 * t5 * t1 * k * (t2 * t2) *
                t3P + k * k * t4 * t1P * t2 * t3 + k * t3P * t5 - k * t1 *
                t3P - t4 * t1P * k + k * k * t3P - k *
                t3P - 5 * k * t3P * t2 - 2 * k * t2 * t5 * t2P * t3 + t5 *
                t2P * (t2 * t2)) * t2aP / (t4 * t1P + t4 *
                t1P * t3 + t4 * t1P * t2 + t4 * t1P * t2 * t3 + t5 *
                t2P + t5 * t2P * t3 + t5 * t2P * t1 + t5 *
                t2P * t1 * t3 + t3P + t3P * t2 +
                t3P * t1 + t3P * t1 * t2 - t3P * t4 -
                t3P * t4 * t2 - t3P * t4 * t1 - t3P * t4 * t1 * t2 -
                t3P * t5 - t3P * t5 * t2 - t3P * t5 * t1 -
                t3P * t5 * t1 * t2) * t2bP;

            if (k > 0)
                test = Math.Abs(A22 / A[2, 2]);

            A[2, 2] += A22;
            k++;

        } while (test > Criteria && k < maxIter);

        if (k == maxIter)
            return SE;

        //A23
        test = 100.0;
        k = 0;
        do
        {
            double t1P = Math.Pow((t1 / (1 + t1)), k);
            double t2P = Math.Pow((t2 / (1 + t2)), k);
            double t3P = Math.Pow((t3 / (1 + t3)), k);

            double A23 = -(1 + t1) * t2P * t5 * (-t2 + k) * (-1 + t4 + t5) *
                t3P * (-t3 + k) / t3 / (1 + t3) / t2 / (1 + t2) / (t4 * t1P + t4 *
                t1P * t3 + t4 * t1P * t2 + t4 * t1P * t2 * t3 + t5 *
                t2P * t3 + t5 * t2P * t1 * t3 + t5 * t2P + t5 *
                t2P * t1 - t3P * t4 - t3P * t4 * t2 +
                t3P + t3P * t2 + t3P * t1 +
                t3P * t1 * t2 - t3P * t4 * t1 - t3P * t4 * t1 * t2 -
                t3P * t5 - t3P * t5 * t2 - t3P * t5 * t1 -
                t3P * t5 * t1 * t2);

            if (k > 0)
                test = Math.Abs(A23 / A[2, 3]);

            A[2, 3] += A23;
            k++;

        } while (test > Criteria && k < maxIter);

        if (k == maxIter)
            return SE;

        //A24
        test = 100.0;
        k = 0;
        do
        {
            double t1P = Math.Pow((t1 / (1 + t1)), k);
            double t2P = Math.Pow((t2 / (1 + t2)), k);
            double t3P = Math.Pow((t3 / (1 + t3)), k);

            double A24 = t5 * t2P * (-t2 + k) * (t1P + t1P * t3 -
                t3P - t3P * t1) / (1 + t2) / (t4 * t1P + t4 *
                t1P * t3 + t4 * t1P * t2 + t4 * t1P * t2 * t3 + t5 *
                t2P * t3 + t5 * t2P * t1 * t3 + t5 * t2P + t5 *
                t2P * t1 - t3P * t4 - t3P * t4 * t2 +
                t3P + t3P * t2 + t3P * t1 +
                t3P * t1 * t2 - t3P * t4 * t1 - t3P * t4 * t1 * t2 -
                t3P * t5 - t3P * t5 * t2 - t3P * t5 * t1 -
                t3P * t5 * t1 * t2) / t2;

            if (k > 0)
                test = Math.Abs(A24 / A[2, 4]);

            A[2, 4] += A24;
            k++;

        } while (test > Criteria && k < maxIter);

        if (k == maxIter)
            return SE;

        //A25
        test = 100.0;
        k = 0;
        do
        {
            double t1P = Math.Pow((t1 / (1 + t1)), k);
            double t2P = Math.Pow((t2 / (1 + t2)), k);
            double t3P = Math.Pow((t3 / (1 + t3)), k);

            double A25 = -t2P * (-t2 + k) * (t3P - t3P * t4 + t4 *
                t1P * t3 + t4 * t1P + t3P * t1 -
                t3P * t4 * t1) / (t4 * t1P + t4 * t1P * t3 + t4 *
                t1P * t2 + t4 * t1P * t2 * t3 + t5 * t2P * t3 + t5 *
                t2P * t1 * t3 + t5 * t2P + t5 * t2P * t1 -
                t3P * t4 - t3P * t4 * t2 + t3P +
                t3P * t2 + t3P * t1 + t3P * t1 * t2 -
                t3P * t4 * t1 - t3P * t4 * t1 * t2 - t3P * t5 -
                t3P * t5 * t2 - t3P * t5 * t1 -
                t3P * t5 * t1 * t2) / t2 / (1 + t2);

            if (k > 0)
                test = Math.Abs(A25 / A[2, 5]);

            A[2, 5] += A25;
            k++;

        } while (test > Criteria && k < maxIter);

        if (k == maxIter)
            return SE;

        //A33
        test = 100.0;
        k = 0;
        do
        {
            double t1P = Math.Pow((t1 / (1 + t1)), k);
            double t2P = Math.Pow((t2 / (1 + t2)), k);
            double t3P = Math.Pow((t3 / (1 + t3)), k);

            double A33 = (-1 + t4 + t5) * t3P * (k * k * t5 * t2P * t1 + k * k * t4 *
                t1P * t2 * t3 - (t3 * t3) * t3P * t5 * t1 - 2 * k * t3 *
                t3P * t1 - 5 * k * t4 * t1P * t3 - k * t4 *
                t1P * t2 - 5 * k * t5 * t2P * t3 + k *
                t3P * t4 * t2 + 2 * (t3 * t3) * t4 * t1P + 2 * t33P * t4 *
                t1P + 2 * t33P * t5 * t2P + 2 * (t3 * t3) * t5 *
                t2P - (t3 * t3) * t3P * t4 + (t3 * t3) *
                t3P * t2 + (t3 * t3) * t3P * t1 - (t3 * t3) *
                t3P * t5 + k * t1 * t3P * t5 + k * t1 *
                t3P * t4 - k * t1 * t5 * t2P + k *
                t3P * t5 * t2 + k * k * t4 * t1P + k * k * t4 *
                t1P * t3 + k * k * t4 * t1P * t2 - (t3 * t3) *
                t3P * t4 * t2 + (t3 * t3) * t3P * t1 * t2 + 2 * t33P * t5 *
                t2P * t1 + 2 * (t3 * t3) * t5 * t2P * t1 - 2 * k * t3 *
                t3P * t2 + 2 * k * t3 * t3P * t4 + 2 * (t3 * t3) * t4 *
                t1P * t2 - t4 * t1P * k + k * t3P * t5 + k *
                t3P * t4 - k * t5 * t2P - k * t1 * t3P + k * k * t5 *
                t2P - k * t3P * t2 - (t3 * t3) * t3P * t5 * t2 - (t3 * t3) *
                t3P * t4 * t1 + 2 * k * t3 * t3P * t5 - 4 * k * (t3 * t3) * t5 *
                t2P - 4 * k * (t3 * t3) * t4 * t1P + 2 * t33P * t4 *
                t1P * t2 - k * t3P + k * k * t5 * t2P * t3 - (t3 * t3) *
                t3P * t4 * t1 * t2 - (t3 * t3) * t3P * t5 * t1 * t2 - 4 * k * (t3 * t3) * t4 *
                t1P * t2 - 4 * k * (t3 * t3) * t5 * t2P * t1 + 2 * k * t3 *
                t3P * t4 * t2 - 2 * k * t3 * t3P * t1 * t2 + 2 * k * t3 *
                t3P * t4 * t1 + 2 * k * t3 * t3P * t4 * t1 * t2 + 2 * k * t3 *
                t3P * t5 * t2 + 2 * k * t3 * t3P * t5 * t1 + 2 * k * t3 *
                t3P * t5 * t1 * t2 - 5 * k * t1 * t5 * t2P * t3 + k * t1 *
                t3P * t4 * t2 + k * t1 * t3P * t5 * t2 + k * k * t5 *
                t2P * t1 * t3 - k * t1 * t3P * t2 + (t3 * t3) *
                t3P - 5 * k * t4 * t1P * t2 * t3 - 2 * k * t3 * t3P) / (t4 *
                t1P + t4 * t1P * t3 + t4 * t1P * t2 + t4 *
                t1P * t2 * t3 + t5 * t2P * t3 + t5 * t2P * t1 * t3 + t5 *
                t2P + t5 * t2P * t1 - t3P * t4 -
                t3P * t4 * t2 + t3P + t3P * t2 +
                t3P * t1 + t3P * t1 * t2 - t3P * t4 * t1 -
                t3P * t4 * t1 * t2 - t3P * t5 - t3P * t5 * t2 -
                t3P * t5 * t1 - t3P * t5 * t1 * t2) * t3bP * t3aP;

            if (k > 0)
                test = Math.Abs(A33 / A[3, 3]);

            A[3, 3] += A33;
            k++;

        } while (test > Criteria && k < maxIter);

        if (k == maxIter)
            return SE;

        //A34
        test = 100.0;
        k = 0;
        do
        {
            double t1P = Math.Pow((t1 / (1 + t1)), k);
            double t2P = Math.Pow((t2 / (1 + t2)), k);
            double t3P = Math.Pow((t3 / (1 + t3)), k);

            double A34 = -t3P * (-t3 + k) * (t5 * t1P - t1P - t2 *
                t1P + t5 * t2 * t1P - t5 * t2P - t5 *
                t2P * t1) / (t4 * t1P + t4 * t1P * t3 + t4 *
                t1P * t2 + t4 * t1P * t2 * t3 + t5 * t2P * t3 + t5 *
                t2P * t1 * t3 + t5 * t2P + t5 * t2P * t1 -
                t3P * t4 - t3P * t4 * t2 + t3P +
                t3P * t2 + t3P * t1 + t3P * t1 * t2 -
                t3P * t4 * t1 - t3P * t4 * t1 * t2 - t3P * t5 -
                t3P * t5 * t2 - t3P * t5 * t1 -
                t3P * t5 * t1 * t2) / t3 / (1 + t3);

            if (k > 0)
                test = Math.Abs(A34 / A[3, 4]);

            A[3, 4] += A34;
            k++;

        } while (test > Criteria && k < maxIter);

        if (k == maxIter)
            return SE;

        //A35
        test = 100.0;
        k = 0;
        do
        {
            double t1P = Math.Pow((t1 / (1 + t1)), k);
            double t2P = Math.Pow((t2 / (1 + t2)), k);
            double t3P = Math.Pow((t3 / (1 + t3)), k);

            double A35 = t3P * (-t3 + k) * (-t4 * t2P + t1 * t2P + t4 *
                t1P + t2P - t4 * t1 * t2P + t4 *
                t1P * t2) / (t4 * t1P + t4 * t1P * t3 + t4 *
                t1P * t2 + t4 * t1P * t2 * t3 + t5 * t2P * t3 + t5 *
                t2P * t1 * t3 + t5 * t2P + t5 * t2P * t1 -
                t3P * t4 - t3P * t4 * t2 + t3P +
                t3P * t2 + t3P * t1 + t3P * t1 * t2 -
                t3P * t4 * t1 - t3P * t4 * t1 * t2 - t3P * t5 -
                t3P * t5 * t2 - t3P * t5 * t1 -
                t3P * t5 * t1 * t2) / t3 / (1 + t3);

            if (k > 0)
                test = Math.Abs(A35 / A[3, 5]);

            A[3, 5] += A35;
            k++;
        } while (test > Criteria && k < maxIter);

        if (k == maxIter)
            return SE;

        //A44
        test = 100.0;
        k = 0;
        do
        {
            double t1P = Math.Pow((t1 / (1 + t1)), k);
            double t2P = Math.Pow((t2 / (1 + t2)), k);
            double t3P = Math.Pow((t3 / (1 + t3)), k);

            double A44 = (1 + t2) * Math.Pow(t1P + t1P * t3 - t3P -
                t3P * t1, 2) / (1 + t3) / (1 + t1) / (t4 * t1P + t4 *
                t1P * t3 + t4 * t1P * t2 + t4 * t1P * t2 * t3 + t5 *
                t2P * t3 + t5 * t2P * t1 * t3 + t5 * t2P + t5 *
                t2P * t1 - t3P * t4 - t3P * t4 * t2 +
                t3P + t3P * t2 + t3P * t1 + t3P * t1 * t2 -
                t3P * t4 * t1 - t3P * t4 * t1 * t2 - t3P * t5 -
                t3P * t5 * t2 - t3P * t5 * t1 - t3P * t5 * t1 * t2);

            if (k > 0)
                test = Math.Abs(A44 / A[4, 4]);

            A[4, 4] += A44;
            k++;

        } while (test > Criteria && k < maxIter);

        if (k == maxIter)
            return SE;

        //A45
        test = 100.0;
        k = 0;
        do
        {
            double t1P = Math.Pow((t1 / (1 + t1)), k);
            double t2P = Math.Pow((t2 / (1 + t2)), k);
            double t3P = Math.Pow((t3 / (1 + t3)), k);

            double A45 = (t1P + t1P * t3 - t3P -
                t3P * t1) * (t2P + t2P * t3 - t3P -
                t3P * t2) / (1 + t3) / (t4 * t1P + t4 * t1P * t3 + t4 *
                t1P * t2 + t4 * t1P * t2 * t3 + t5 * t2P * t3 + t5 *
                t2P * t1 * t3 + t5 * t2P + t5 * t2P * t1 -
                t3P * t4 - t3P * t4 * t2 + t3P + t3P * t2 +
                t3P * t1 + t3P * t1 * t2 - t3P * t4 * t1 -
                t3P * t4 * t1 * t2 - t3P * t5 - t3P * t5 * t2 -
                t3P * t5 * t1 - t3P * t5 * t1 * t2);

            if (k > 0)
                test = Math.Abs(A45 / A[4, 5]);

            A[4, 5] += A45;
            k++;

        } while (test > Criteria && k < maxIter);

        if (k == maxIter)
            return SE;

        //A55
        test = 100.0;
        k = 0;
        do
        {
            double t1P = Math.Pow((t1 / (1 + t1)), k);
            double t2P = Math.Pow((t2 / (1 + t2)), k);
            double t3P = Math.Pow((t3 / (1 + t3)), k);

            double A55 = (1 + t1) * Math.Pow(t2P + t2P * t3 - t3P -
                t3P * t2, 2) / (1 + t3) / (1 + t2) / (t4 * t1P + t4 *
                t1P * t3 + t4 * t1P * t2 + t4 * t1P * t2 * t3 + t5 *
                t2P * t3 + t5 * t2P * t1 * t3 + t5 * t2P + t5 *
                t2P * t1 - t3P * t4 - t3P * t4 * t2 +
                t3P + t3P * t2 + t3P * t1 + t3P * t1 * t2 -
                t3P * t4 * t1 - t3P * t4 * t1 * t2 - t3P * t5 -
                t3P * t5 * t2 - t3P * t5 * t1 - t3P * t5 * t1 * t2);

            if (k > 0)
                test = Math.Abs(A55 / A[5, 5]);

            A[5, 5] += A55;
            k++;

        } while (test > Criteria && k < maxIter);

        if (k == maxIter)
            return SE;

        SE = MatrixInversion(5, sHatSubset, a00, a0, A, ref SEFlag);

        return SE;
    }
    
	//*********************************************************************
	//**           Calculate Standard Error for 2 Mixed Exp             ***
	//*********************************************************************
	public static double SETwoMixedExp(double t1, double t2, double t3, double
		sHatSubset, ref int SEFlag)
	{
		double a00 = -(-t3 * t2 + t3 * t1 + t2 + t1 * t2) /
			(-t3 * t2 - 1 - t1 + t3 * t1);

		double[] a0 = new double[4];
		a0[1] = -t3 * (1 + t2) / (1 + t1) / (-t3 * t2 - 1 - t1 + t3 * t1);
		a0[2] = (t3 - 1 - t1 + t3 * t1) / (1 + t2) / (-t3 * t2 - 1 - t1 + t3 * t1);
		a0[3] = -1 / (-t3 * t2 - 1 - t1 + t3 * t1) * (-t2 + t1);

		double[,] A = new double[4, 4];
        double SE = 0.0;

		//A11
		double test = 100.0;
		int k = 0;
		do
		{
            double A11=-t3*Math.Pow((t1/(1+t1)),k)*(t3*Math.Pow((t1/(1+t1)),k)*k+2*k*t1*t3*
                Math.Pow((t1/(1+t1)),k)*t2-(t1*t1)*t3*Math.Pow((t1/(1+t1)),k)*t2+2*k*t1*t3*
                Math.Pow((t1/(1+t1)),k)-5*k*t1*Math.Pow(t2/(1+t2),k)*t3-4*k*(t1*t1)*
                Math.Pow(t2/(1+t2),k)*t3+k*k*Math.Pow(t2/(1+t2),k)*t3*t1+k*t3*
                Math.Pow((t1/(1+t1)),k)*t2-(t1*t1)*t3*Math.Pow((t1/(1+t1)),k)+2*(t1*t1)*
                Math.Pow(t2/(1+t2),k)*t3+2*Math.Pow(t1,3)*Math.Pow(t2/(1+t2),k)*t3+5*k*t1*
                Math.Pow(t2/(1+t2),k)+4*k*(t1*t1)*Math.Pow(t2/(1+t2),k)+k*k*Math.Pow(t2/(1+t2),k)*t3-k*k*
                Math.Pow(t2/(1+t2),k)*t1-k*Math.Pow(t2/(1+t2),k)*t3-2*(t1*t1)*Math.Pow(t2/(1+t2),k)-2*
                Math.Pow(t1,3)*Math.Pow(t2/(1+t2),k)-k*k*Math.Pow(t2/(1+t2),k)+k*Math.Pow(t2/(1+t2),k))/(-t3*
                Math.Pow((t1/(1+t1)),k)-t3*Math.Pow((t1/(1+t1)),k)*t2+Math.Pow(t2/(1+t2),k)*t3-Math.Pow(t2/(1+t2),k)-
                Math.Pow(t2/(1+t2),k)*t1+Math.Pow(t2/(1+t2),k)*t3*t1)*Math.Pow(t1,(-2))*Math.Pow((1 + t1), (-3));

			if (k > 0)
				test = Math.Abs(A11 / A[1, 1]);

			A[1, 1] += A11;
			k++;

		} while (test > Criteria && k < maxIter);

        if (k == maxIter)
            return SE;

		//A12
		test = 1.0;
		k = 0;
		do
		{
			double A12 = Math.Pow((t1/(1+t1)),k)*t3*(-t1+k)*(-1+t3)*
				Math.Pow((t2/(1+t2)),k)*(-t2+k)/t2/(1+t2)/t1/(1+t1)/(-t3*Math.Pow((t1/(1+t1)),k)-t3*
				Math.Pow((t1/(1+t1)),k)*t2+Math.Pow((t2/(1+t2)),k)*t3-Math.Pow((t2/(1+t2)),k)-
				Math.Pow((t2/(1+t2)),k)*t1+Math.Pow((t2/(1+t2)),k)*t3*t1);

			if (k > 0)
				test = Math.Abs(A12 / A[1, 2]);

			A[1, 2] += A12;
			k++;

		} while (test > Criteria && k < maxIter);

         if (k == maxIter)
            return SE;

		//A13
		test = 1.0;
		k = 0;
		do
		{
			double A13 = Math.Pow((t1/(1+t1)),k)*(-t1+k)*Math.Pow((t2/(1+t2)),k)/t1/(1+t1)/(-t3*
				Math.Pow((t1/(1+t1)),k)-t3*Math.Pow((t1/(1+t1)),k)*t2+Math.Pow((t2/(1+t2)),k)*t3-
				Math.Pow((t2/(1+t2)),k)-Math.Pow((t2/(1+t2)),k)*t1+Math.Pow((t2/(1+t2)),k)*t3*t1);

			if (k > 0)
				test = Math.Abs(A13 / A[1, 3]);

			A[1, 3] += A13;
			k++;

		} while (test > Criteria && k < maxIter);

        if (k == maxIter)
            return SE;

		//A22
		test = 1.0;
		k = 0;
		do
		{
			double A22 = (-1+t3)*Math.Pow((t2/(1+t2)),k)*(-k*t1*Math.Pow((t2/(1+t2)),k)+k*
				Math.Pow((t2/(1+t2)),k)*t3-t3*Math.Pow(t1/(1+t1),k)*k+k*k*t3*Math.Pow(t1/(1+t1),k)-2*k*t2*
				Math.Pow((t2/(1+t2)),k)+(t2*t2)*Math.Pow((t2/(1+t2)),k)*t1-(t2*t2)*Math.Pow((t2/(1+t2)),k)*t3+2*
				Math.Pow(t2,3)*t3*Math.Pow(t1/(1+t1),k)+2*(t2*t2)*t3*Math.Pow(t1/(1+t1),k)+2*k*t2*
				Math.Pow((t2/(1+t2)),k)*t3-4*k*(t2*t2)*t3*Math.Pow(t1/(1+t1),k)-5*k*t3*
				Math.Pow(t1/(1+t1),k)*t2-(t2*t2)*Math.Pow((t2/(1+t2)),k)*t3*t1-2*k*t2*
				Math.Pow((t2/(1+t2)),k)*t1-k*Math.Pow((t2/(1+t2)),k)+(t2*t2)*Math.Pow((t2/(1+t2)),k)+k*k*t3*
				Math.Pow(t1/(1+t1),k)*t2+2*k*t2*Math.Pow((t2/(1+t2)),k)*t3*t1+k*t1*
				Math.Pow((t2/(1+t2)),k)*t3)/(t3*Math.Pow(t1/(1+t1),k)+t3*Math.Pow(t1/(1+t1),k)*t2-
				Math.Pow((t2/(1+t2)),k)*t3+Math.Pow((t2/(1+t2)),k)+Math.Pow((t2/(1+t2)),k)*t1-
				Math.Pow((t2/(1+t2)),k)*t3*t1)*Math.Pow(t2,(-2))*Math.Pow((1+t2),(-3));

			if (k > 0)
				test = Math.Abs(A22 / A[2, 2]);

			A[2, 2] += A22;
			k++;

		} while (test > Criteria && k < maxIter);

        if (k == maxIter)
            return SE;

		//A23
		test = 1.0;
		k = 0;
		do
		{
			double A23 = Math.Pow((t2/(1+t2)),k)*(-t2+k)*Math.Pow((t1/(1+t1)),k)/(t3*Math.Pow((t1/(1+t1)),k)+t3*
				Math.Pow((t1/(1+t1)),k)*t2-Math.Pow((t2/(1+t2)),k)*t3+Math.Pow((t2/(1+t2)),k)+
				Math.Pow((t2/(1+t2)),k)*t1-Math.Pow((t2/(1+t2)),k)*t3*t1)/t2/(1+t2);

			if (k > 0)
				test = Math.Abs(A23 / A[2, 3]);

			A[2, 3] += A23;
			k++;

		} while (test > Criteria && k < maxIter);

        if (k == maxIter)
            return SE;

		//A33
		test = 1.0;
		k = 0;
		do
		{
			double A33 = Math.Pow(Math.Pow((t1 / (1 + t1)), k) + Math.Pow((t1 / (1 + t1)), k) * t2 - Math.Pow(t2 / (1 + t2), k) -
				Math.Pow(t2 / (1 + t2), k) * t1, 2) / (1 + t2) / (1 + t1) / (t3 * Math.Pow((t1 / (1 + t1)), k) + t3 *
				Math.Pow((t1 / (1 + t1)), k) * t2 - Math.Pow(t2 / (1 + t2), k) * t3 + Math.Pow(t2 / (1 + t2), k) + Math.Pow(t2 / (1 + t2), k) * t1 -
				Math.Pow(t2 / (1 + t2), k) * t3 * t1);

			if (k > 0)
				test = Math.Abs(A33 / A[3, 3]);

			A[3, 3] += A33;
			k++;

		} while (test > Criteria && k < maxIter);

        if (k == maxIter)
            return SE;
		
        SE = MatrixInversion(3, sHatSubset, a00, a0, A, ref SEFlag);

		return SE;
	}

	//*********************************************************************
	//**             Three Mixed Exponential Fits                       ***
	//*********************************************************************
	public int ThreeMixedExponentialFits(int r, ref double u1,
		ref double u2, ref double mlesSExp1, ref double mlesSExp2,
		ref double mlesSExp3, ref double mlesSExp4, ref double mlesSExp5, 
		ref int fitsCheck, ref double[] fitsCount)
	{
		int MLEFlag = MLEThreeMixedExp(r, ref u1, ref u2,
				ref mlesSExp1, ref mlesSExp2, ref mlesSExp3);

		if (MLEFlag == 1)
		{
			double denom = (mlesSExp1 * mlesSExp2 * mlesSExp3) +
					(u2 * mlesSExp1 * mlesSExp3) + (u1 * mlesSExp2 * mlesSExp3) -
					(u2 * mlesSExp1 * mlesSExp2) - (u1 * mlesSExp1 * mlesSExp2) +
					(mlesSExp1 * mlesSExp2);

			mlesSExp4 = ((1.0 + mlesSExp1) * u1 * mlesSExp2 * mlesSExp3) /
				denom;

			mlesSExp5 = ((1.0 + mlesSExp2) * u2 * mlesSExp1 * mlesSExp3) /
				denom;	

			fitsCheck = 1;
			for (int t = 1; t <= freq[r]; t++)
			{
				fitsCount[t] = s[r] *
					((u1 * ((1.0 / mlesSExp1) * Math.Pow((mlesSExp1 / (1.0 + mlesSExp1)), t))) +
					 (u2 * ((1.0 / mlesSExp2) * Math.Pow((mlesSExp2 / (1.0 + mlesSExp2)), t))) +
					((1.0 - u1 - u2) * ((1.0 / mlesSExp3) *
					Math.Pow((mlesSExp3 / (1.0 + mlesSExp3)), t))));

				//if fits is negative, throw flag for bad fits
				if (fitsCount[t] < 0.0)
					fitsCheck = 0;
			}
		}
		return MLEFlag;
	}


	//*********************************************************************
	//**            Three Mixed Exponential Model                       ***
	//*********************************************************************
	public void ThreeMixedExponentialModel(string modelName,
		StreamWriter swAnalysis, int fMin, int numParams, int modelNum, 
        double maxGOF, int[] bestCount)
	{
		string[] T = new string[7];

		//find fmin frequency
		int r = 1;
		while (freq[r] < fMin)
			r++;
		int freqMin = r;

		for (r = freqMin; r <= obsMax; r++)
		{
         double u1 = 0.0;
         double u2 = 0.0;
         double mlesSExp1 = 0.0;
         double mlesSExp2 = 0.0;
         double mlesSExp3 = 0.0;
			double mlesSExp4 = 0.0;
			double mlesSExp5 = 0.0;
			int fitsCheck = 1;

			double[] fitsCount = new double[freq[r] + 1];

			int MLEFlag = ThreeMixedExponentialFits(r, ref u1, ref u2,
				ref  mlesSExp1, ref  mlesSExp2, ref mlesSExp3, ref mlesSExp4,
				ref  mlesSExp5, ref  fitsCheck, ref fitsCount);

			if (MLEFlag == 1 && fitsCheck == 1)
         {
            //compute fitted values 
				int extendedTau = freq[r] * 4;
            double[] fitsExtended = new double[extendedTau + 1];

				for (int t = 1; t <= freq[r]; t++)
					fitsExtended[t] = fitsCount[t];

            //compute extended fitted values 
            for (int t = (freq[r] + 1); t <= extendedTau; t++)
               fitsExtended[t] = s[r] *
               ((u1 * ((1.0 / mlesSExp1) * Math.Pow((mlesSExp1 / (1.0 + mlesSExp1)), t))) +
                (u2 * ((1.0 / mlesSExp2) * Math.Pow((mlesSExp2 / (1.0 + mlesSExp2)), t))) +
               ((1.0 - u1 - u2) * ((1.0 / mlesSExp3) * 
               Math.Pow((mlesSExp3 / (1.0 + mlesSExp3)), t))));

            double sHatSubset = s[r] * (((1.0 + mlesSExp1) * (1.0 + mlesSExp2) *
               (1.0 + mlesSExp3)) / ((-mlesSExp5 * mlesSExp3 * mlesSExp1) +
               (mlesSExp1 * mlesSExp2 * mlesSExp3) + (mlesSExp5 * mlesSExp1 * mlesSExp2) +
               (mlesSExp4 * mlesSExp1 * mlesSExp2) + mlesSExp3 -
               (mlesSExp4 * mlesSExp3 * mlesSExp2) + (mlesSExp3 * mlesSExp1) +
               (mlesSExp3 * mlesSExp2) - (mlesSExp4 * mlesSExp3) - (mlesSExp5 * mlesSExp3) +
               (mlesSExp4 * mlesSExp1) + (mlesSExp5 * mlesSExp2)));

            double sHatTotal = sHatSubset + (s[obsMax] - s[r]);

            double part1 = lnSFactorial[r] - sumlnFFactorial[r];

            double part2 = 0.0;
            for (int t = 1; t <= r; t++)
            {
               part2 += (observedCount[t] * Math.Log(
               (u1 * ((1.0 / mlesSExp1) * Math.Pow((mlesSExp1 / (1.0 + mlesSExp1)), freq[t]))) +
               (u2 * ((1.0 / mlesSExp2) * Math.Pow((mlesSExp2 / (1.0 + mlesSExp2)), freq[t]))) +
               ((1.0 - u1 - u2) * ((1.0 / mlesSExp3) * 
               Math.Pow((mlesSExp3 / (1.0 + mlesSExp3)), freq[t])))));
            }

            double AIC = 0.0;
            double AICc = 0.0;
            double chiSqAll = 0.0;
            double GOF0 = 0.0;
            double GOF5 = 0.0;

            int AICcCheck = 0;
            int GOF0Check = 0;
            int GOF5Check = 0;

				GOF5Test[modelNum, r] = CalculateAnalysisVariables(part1, part2,
					numParams, r,fitsCount, fitsExtended, maxGOF, ref AIC, 
					ref AICc,ref AICcCheck, ref chiSqAll, ref GOF0, ref GOF5,
               ref GOF0Check, ref GOF5Check, modelNum);

            //Standard Error
            int SEFlag = 0;
            double SE = SEThreeMixedExp(mlesSExp1, mlesSExp2, mlesSExp3,
               mlesSExp4, mlesSExp5, sHatSubset, ref SEFlag);

            //Confidence Bounds
            double LCB = 0.0;
            double UCB = 0.0;
            int boundsCheck = 0;
            if (SEFlag == 1)
               boundsCheck = GetConfidenceBounds(r, SE, sHatSubset, 
                  ref LCB, ref UCB);


            //Write out Analysis row for this dataset/model/tau
            T[0] = mlesSExp1.ToString();
            T[1] = mlesSExp2.ToString();
            T[2] = mlesSExp3.ToString();
            T[3] = mlesSExp4.ToString();
            T[4] = mlesSExp5.ToString();

            WriteAnalysisRow(swAnalysis, modelName, r, sHatTotal,
                  SEFlag, SE, boundsCheck, LCB, UCB, GOF0Check, GOF0,
                  GOF5Check, GOF5, AICcCheck, AICc, T, chiSqAll, AIC,
                  modelNum, bestCount);
      }
      else
         //Write out Analysis row as all missing
         swAnalysis.WriteLine("{0},{1},{2},,,,,,,,,,,,,,,,",
            modelName, freq[r], s[r]);
	   }
	}

	//*********************************************************************
	//**              Two Mixed Exponential Fits                        ***
	//*********************************************************************
	public int TwoMixedExponentialFits(int r, ref double u,  
		ref double mlesSExp1, ref double mlesSExp2, ref double mlesSExp3,
		ref int fitsCheck, ref double[] fitsCount)
	{
		int MLEFlag = MLETwoMixedExp(r, ref u, ref mlesSExp1, ref mlesSExp2);

		if (MLEFlag == 1)
		{
			mlesSExp3 = (u * mlesSExp2 * (1.0 + mlesSExp1)) /
				(mlesSExp1 + (mlesSExp1 * mlesSExp2) + (u * mlesSExp2) -
				(u * mlesSExp1));
	
			fitsCheck = 1;

			for (int t = 1; t <= freq[r]; t++)
			{
				fitsCount[t] = s[r] * ((u * ((1.0 / mlesSExp1) *
					Math.Pow((mlesSExp1 / (1.0 + mlesSExp1)), t))) +
					((1.0 - u) * ((1.0 / mlesSExp2) *
					Math.Pow((mlesSExp2 / (1.0 + mlesSExp2)), t))));

				//if fits is negative, throw flag for bad fits
				if (fitsCount[t] < 0.0)
					fitsCheck = 0;
			}
		}
		return MLEFlag;
	}
    
    //*********************************************************************
	//**              Two Mixed Exponential Model                       ***
	//*********************************************************************
	public void TwoMixedExponentialModel(string modelName,
		StreamWriter swAnalysis, int fMin, int numParams, int modelNum, 
        double maxGOF, int[] bestCount)
	{
		string[] T = new string[7];

		//find fmin frequency
		int r = 1;
		while (freq[r] < fMin)
			r++;
		int freqMin = r;

		for (r = freqMin; r <= obsMax; r++)
		{
			double u = 0.0; ;
			double mlesSExp1 = 0.0;
			double mlesSExp2 = 0.0;
			double mlesSExp3 = 0.0;
			int fitsCheck = 1;

			double[] fitsCount = new double[freq[r] + 1];

			int MLEFLAG = TwoMixedExponentialFits(r, ref u, ref mlesSExp1,
				ref mlesSExp2, ref mlesSExp3, ref fitsCheck, ref  fitsCount);

			if (MLEFLAG == 1 && fitsCheck == 1)
			{
				//compute fitted values 
				int extendedTau = freq[r] * 4;
				double[] fitsExtended = new double[extendedTau + 1];

				for (int t = 1; t <= freq[r]; t++)
					fitsExtended[t] = fitsCount[t];

				//compute extended fitted values 
				for (int t = (freq[r] + 1); t <= extendedTau; t++)
					fitsExtended[t] = s[r] * ((mlesSExp3 * ((1.0 / mlesSExp1) *
					Math.Pow((mlesSExp1 / (1.0 + mlesSExp1)), t))) +
					((1.0 - mlesSExp3) * ((1.0 / mlesSExp2) *
					Math.Pow((mlesSExp2 / (1.0 + mlesSExp2)), t))));

				//estimated total species				
				double sHatSubset = s[r] * ((1.0 + mlesSExp1) * (1.0 + mlesSExp2) /
					(mlesSExp2 + (mlesSExp1 * mlesSExp2) - (mlesSExp3 * mlesSExp2) +
					(mlesSExp3 * mlesSExp1)));

				double sHatTotal = sHatSubset + (s[obsMax] - s[r]);

				double part1 = lnSFactorial[r] - sumlnFFactorial[r];

				double part2 = 0.0;
				for (int t = 1; t <= r; t++)
				{
					part2 += (observedCount[t] * Math.Log((u * ((1.0 / mlesSExp1) *
					Math.Pow((mlesSExp1 / (1.0 + mlesSExp1)), freq[t]))) +
					((1.0 - u) * ((1.0 / mlesSExp2) *
					Math.Pow((mlesSExp2 / (1.0 + mlesSExp2)), freq[t])))));
				}

				double AIC = 0.0;
				double AICc = 0.0;
				double chiSqAll = 0.0;
				double GOF0 = 0.0;
				double GOF5 = 0.0;

				int AICcCheck = 0;
				int GOF0Check = 0;
				int GOF5Check = 0;

				GOF5Test[modelNum, r] = CalculateAnalysisVariables(part1, part2, 
					numParams, r,fitsCount, fitsExtended, maxGOF, ref AIC, 
					ref AICc, ref AICcCheck, ref chiSqAll, ref GOF0, ref GOF5,
					ref GOF0Check, ref GOF5Check, modelNum);

				//Standard Error
				int SEFlag = 0;
				double SE = SETwoMixedExp(mlesSExp1, mlesSExp2, mlesSExp3,
					sHatSubset, ref SEFlag);

				//Confidence Bounds
				double LCB = 0.0;
				double UCB = 0.0;
				int boundsCheck = 0;
				if (SEFlag == 1)
					boundsCheck = GetConfidenceBounds(r, SE, sHatSubset,
						ref LCB, ref UCB);

				//Write out Analysis row for this dataset/model/tau
				T[0] = mlesSExp1.ToString();
				T[1] = mlesSExp2.ToString();
				T[2] = mlesSExp3.ToString();
				WriteAnalysisRow(swAnalysis, modelName, r, sHatTotal,
						SEFlag, SE, boundsCheck, LCB, UCB, GOF0Check, GOF0,
						GOF5Check, GOF5, AICcCheck, AICc, T, chiSqAll, AIC,
						modelNum, bestCount);
			}
			else
				//Write out Analysis row as all missing
				swAnalysis.WriteLine("{0},{1},{2},,,,,,,,,,,,,,,,",
					modelName, freq[r], s[r]);
		}
	}

    //*********************************************************************
    //**         Weighted Linear Regression Fits                        ***
    //*********************************************************************
    public void WLRFits(int r, ref double gamma,
        ref double delta, ref double MSE, ref double k, ref int fitsCheck,
        ref double[] fitsCount)
    {
        //calculate k
        for (int j = 1; j < r; j++)
        {
            double temp = 0.0;
            for (int i = 1; i < r; i++)
                temp += i * w[i] * (i - j);
            k += w[j] * temp;
        }

        //calculate gamma
        for (int j = 1; j < r; j++)
        {
            double temp = 0.0;
            for (int i = 1; i < r; i++)
                temp += (j - i) * w[i] * y[i];
            gamma += j * w[j] * temp;
        }
        gamma /= k;

        //calculate delta
        for (int j = 1; j < r; j++)
        {
            double temp = 0.0;
            for (int i = 1; i < r; i++)
                temp += (i - j) * w[i] * y[i];
            delta += w[j] * temp;
        }
        delta /= k;

        //calculate MSE
        for (int j = 1; j < r; j++)
            MSE += (w[j] * (y[j] - gamma - delta * j) * (y[j] -
                gamma - delta * j));
        MSE *= (1.0 / (r - 3.0));

        fitsCount[0] = observedCount[freq[1]] / gamma;
        fitsCount[1] = observedCount[freq[1]];
        for (int t = 2; t <= r; t++)
        {
            fitsCount[t] = fitsCount[t - 1] * (gamma + delta * (t - 1.0)) / t;

            //if fits is negative, throw flag for bad fits
            if (fitsCount[t] < 0.0)
                fitsCheck = 0;
        }
    }

    //*********************************************************************
    //**            Weighted Linear Regression Model                    ***
    //*********************************************************************
    public void WLRModel(string modelName, StreamWriter swAnalysis,
    int fMin, int numParams, int modelNum, double maxGOF,
    int[] bestCount)
    {
        string[] T = new string[7];

        //fmin frequency has to be fMin since frequencies have to be contiguous                
        int freqMin = fMin;

        for (int r = freqMin; r <= freqMax; r++)
        {
            double gamma = 0.0;
            double delta = 0.0;
            double MSE = 0.0;
            double k = 0.0;

            int fitsCheck = 1;

            double[] fitsCount = new double[freq[r] + 1];

            WLRFits(r, ref gamma, ref delta, ref MSE, ref k,
                ref fitsCheck, ref fitsCount);

            //proceed if no negative fits calculated
            if (fitsCheck == 1)
            {
                //estimated total species			
                double sHatSubset = fitsCount[0] + s[r];

                double sHatTotal = sHatSubset + (s[obsMax] - s[r]);

                double AIC = 0.0;
                double AICc = 0.0;

                //calculate Goodness of Fit
                double chiSqAll = ChiSq(r, fitsCount, modelNum);

                int df = freq[r] - numParams;
                int flag = 1;
                double GOF0 = 0.0;
                double test = (chiSqAll - df) / Math.Sqrt(2.0 * df);
		        if (test < maxGOF && chiSqAll < BigChiSq)
			        GOF0 = GoodNessOfFit(chiSqAll, df, ref flag);
		        else
			        flag = 0;
		        int GOF0Check = flag;



                //WLRM Switch
                if (gamma > 0.0 && (delta > 0.0 && delta < 1.0) &&
                    GOF0 > WLRMGOF0[freq[r]])
                    WLRMSwitch[freq[r]] = 1;

                int GOF5Check = 0;
                int AICcCheck = 0;

                GOF5Test[modelNum, r] = 0.0;
                double GOF5 = 0.0;

                //Standard Error
                double varGamma = 0.0;
                for (int i = 1; i < r; i++)
                    varGamma += (i * i * w[i]);
                varGamma *= MSE / k;

                double SE = (s[r] * fitsCount[0] / sHatSubset) +
                   ((observedCount[freq[1]] / gamma) * (observedCount[freq[1]] / gamma) *
                    ((varGamma / (gamma * gamma)) + (1.0 / observedCount[freq[1]])));

                int SEFlag = 0;

                if (SE > 0.0)
                {
                    SE = Math.Sqrt(SE);
                    SEFlag = 1;
                }

                //Confidence Bounds
                double LCB = 0.0;
                double UCB = 0.0;
                int boundsCheck = 0;

                if (SEFlag == 1)
                    boundsCheck = GetConfidenceBounds(r, SE, sHatSubset,
                        ref LCB, ref UCB);

                //Write out Analysis row for this dataset/model/tau
                T[0] = gamma.ToString();
                T[1] = delta.ToString();
                T[2] = MSE.ToString();
                WriteAnalysisRow(swAnalysis, modelName, r, sHatTotal,
                    SEFlag, SE, boundsCheck, LCB, UCB, GOF0Check, GOF0,
                    GOF5Check, GOF5, AICcCheck, AICc, T, chiSqAll, AIC,
                    modelNum, bestCount);
            }
            else
                //Write out Analysis row as all missing
                swAnalysis.WriteLine("{0},{1},{2},,,,,,,,,,,,,,,,,,",
                    modelName, freq[r], s[r]);
        }
    }
    
    
    //*********************************************************************
	//**                  Write out Analysis files                      ***
	//*********************************************************************
	public void WriteAnalysisRow(StreamWriter swAnalysis, string modelName,
		int r, double sHatTotal, int SEFlag, double SE, int boundsCheck,
		double LCB, double UCB, int GOF0Check, double GOF0, int GOF5Check,
		double GOF5, int AICcCheck, double AICc, string[] T, double chiSqAll,
		double AIC, int modelNum, int[] bestCount)
	{
		//Write out Analysis row for this dataset/model/tau
		swAnalysis.Write("{0},{1},{2},{3}",
			modelName, freq[r], s[r], sHatTotal);

		if (SEFlag == 1)
			swAnalysis.Write(",{0}", SE);
		else
			swAnalysis.Write(",");

		if (boundsCheck == 1)
			swAnalysis.Write(",{0},{1}", LCB, UCB);
		else
			swAnalysis.Write(",,");

		//need chisq cutoff ---when it is too big or NAN
		if (Double.IsNaN(chiSqAll))
			swAnalysis.Write(",");
		else
			swAnalysis.Write(",{0}", chiSqAll);

		if (GOF0Check == 1)
			swAnalysis.Write(",{0}", GOF0);
		else
			swAnalysis.Write(",");

		if (GOF5Check == 1)
			swAnalysis.Write(",{0}", GOF5);
		else
			swAnalysis.Write(",");

        //No AIC for Weighted Linear Regression
        if (modelNum != 6 && modelNum != 7)
            swAnalysis.Write(",{0}", AIC);
        else
            swAnalysis.Write(",");

		if (AICcCheck == 1)
			swAnalysis.Write(",{0}", AICc);
		else
			swAnalysis.Write(",");


	   for (int t = 0; t < 7; t++)
			swAnalysis.Write(",{0}", T[t]);

	     swAnalysis.WriteLine(",");

	     int flag = 1;

	    //make sure data is at least there
	    if (GOF5Check == 0 || GOF0Check == 0)
		    flag = 0;

		 //different criteria for weight linear regression
        if ((modelNum == 6 || modelNum == 7) && GOF0Check == 1)
            flag = 1;

		//Bubble Plot file
        if (modelNum != 7)
        {
            bubblePlotDataFlag[modelNum, r] = flag;
            bubblePlotData[modelNum, r] = sHatTotal.ToString() + "," +
                SE.ToString();
        }

        //choose the better WLRM model for this tau
        else if (modelNum == 7 && WLRMSwitch[r] == 1)
        {
            bubblePlotDataFlag[6, r] = flag;
            bubblePlotData[6, r] = sHatTotal.ToString() + "," +
                SE.ToString();
        }

		//Loose Filters for "best" models
		//Eliminate model*tau combinations where GOF5 < 0.001 and SE > estimate 
		if (flag == 1)
			if (GOF5 < 0.001 || SEFlag == 0 || SE > sHatTotal)
		      flag = 0;

        //apply ACE1Tau10Rule
        if (flag == 1 && sHatTotal < ACE1Tau10Rule)
        {
			bestGOF0[0, modelNum, r] = GOF0;
			bestAICc[0, modelNum, r] = AICc;

            //don't use weighted linear regression in counts
            if (modelNum < 6)
                bestCount[0]++;

			//Strict filters
			//Eliminate model*tau combinations where GOF5 < 0.01 and SE > estimate/2. 
			if (GOF5 < 0.01 || SE > (0.5 * sHatTotal))
				flag = 0;

			if (flag == 1)
			{
				bestGOF0[1, modelNum, r] = GOF0;
				bestAICc[1, modelNum, r] = AICc;

                //don't use weighted linear regression in counts
                if (modelNum < 6)
                    bestCount[1]++;
			}

			//L2 filters
			//Eliminate model*tau combinations where GOF5 < 0.001 and SE > estimate/2. 			
			if (GOF5 < 0.001 || SE > (0.5 * sHatTotal))
				flag = 0;
			else
				flag = 1;

			if (flag == 1)
			{
				bestGOF0[2, modelNum, r] = GOF0;
				bestAICc[2, modelNum, r] = AICc;
                //don't use weighted linear regression in counts
                if (modelNum < 6)
                   bestCount[2]++;
			}
		}

        //Loosest Criteria, based on test for GOF5
        if (sHatTotal < ACE1Tau10Rule)
        {
            bestAICc[3, modelNum, r] = AICc;

            //don't use weighted linear regression in counts
            if (modelNum < 6)
                bestCount[3]++;
        }

        //Criteria for Weighted Linear Regression
        if (modelNum == 6 || modelNum == 7)
            bestGOF0[0, modelNum, r] = GOF0;
	}


	//*********************************************************************
	//**              Write out analysis file for Best Models           ***
	//*********************************************************************
	public void WriteBestModelsAnalysisFile(string outputFileName,
		int[,] bestModelTau, string[] modelDescription, double cvrare)
   {
	   StreamWriter swBestModels = new StreamWriter(outputFileName +
			"_BestModelsAnalysis.csv");

      //Write column headers for parametric models
		swBestModels.Write("Total Number of Observed Species = {0},Model,Tau,",
				s[obsMax].ToString());
      swBestModels.Write("Observed Sp,Estimated Total Sp,SE,Lower CB,");
      swBestModels.WriteLine("Upper CB,GOF0,GOF5");

		//Open analysis file
		FileInfo fi = new FileInfo(outputFileName + "_Analysis.csv");

		int[] foundAnalysis = new int[10];
		string[] bestAnalysis = new string[10];

		for (int bm = 0; bm < 10; bm++)
		{
			StreamReader srBest = new StreamReader(fi.OpenRead());

			//Read in column headers
			string data = srBest.ReadLine();

			//Read in first row of data
			data = srBest.ReadLine();
			while (data != null && foundAnalysis[bm] == 0)
			{
				string[] analysisData = data.Split(',');

				if ((analysisData[0].ToString().
					CompareTo(modelDescription[bestModelTau[bm, 0]]) == 0) &&
					analysisData[1].ToString().
					CompareTo(freq[bestModelTau[bm, 1]].ToString()) == 0)
				{
					foundAnalysis[bm] = 1;
					bestAnalysis[bm] = data;
				}
				data = srBest.ReadLine();
			}
			srBest.Close();
		}

      //Check that Parm Max Tau has data--look at estimated species
      if (foundAnalysis[0] == 1)
      {
         string[] ParmMaxTauData = bestAnalysis[7].Split(',');
         if (String.IsNullOrEmpty(ParmMaxTauData[3]))
         {
            foundAnalysis[7] = 0;
            int r = obsMax - 1;

            while (foundAnalysis[7] == 0)
            {
               StreamReader srBest = new StreamReader(fi.OpenRead());

               //Read in column headers
               string data = srBest.ReadLine();

               //Read in first row of data
               data = srBest.ReadLine();
               while (data != null && foundAnalysis[7] == 0)
               {
                  string[] analysisData = data.Split(',');

                  //at correct model
                  if ((analysisData[0].ToString().CompareTo(modelDescription[bestModelTau[7, 0]]) == 0) &&
                     ((analysisData[1].ToString().CompareTo(freq[r].ToString()) == 0)) &&
                     (!String.IsNullOrEmpty(analysisData[3])))
                  {
                        foundAnalysis[7] = 1;
                        bestAnalysis[7] = data;
                  }
                  data = srBest.ReadLine();
               }
               srBest.Close();
               r--;
            }
         }
      }

      string[] bestDescription = {"Best Parm Model", "Parm Model 2a  ", 
	                              "Parm Model 2b  ", "Parm Model 2c  ", 
                                 "WLRM           ", "Non-P 1        ",
                                 "Non-P 2        ", "Parm Max Tau   ",
                                 "WLRM Max Tau   ", "Non-P 3        ",
                                 "Best Discounted"};

      double [] T = new double [7];
      double sHatTotal = 0.0;
      double SE = 0.0;

      //Write out best analysis data for parametric models
		for (int bm = 0; bm < 10; bm++)
		{
         if (bm < 5 || bm == 7 || bm == 8)
         {
            swBestModels.Write("{0},", bestDescription[bm]);

            if (foundAnalysis[bm] == 1)
            {
               string[] analysis = bestAnalysis[bm].Split(',');

               //strip off WLR from description      
               if (bm == 4 || bm == 8)
                  analysis[0] = analysis[0].Remove(analysis[0].Length - 3);

               for (int i = 0; i <= 2; i++)
                  swBestModels.Write("{0},", analysis[i]);

               for (int i = 3; i <= 6; i++)
               {
                  if (!String.IsNullOrEmpty(analysis[i]))
                     swBestModels.Write("{0:F1},", Convert.ToDouble(analysis[i]));
                  else
                     swBestModels.Write(",");
               }

               // certain stats not available for non parametric models             
               for (int i = 8; i <= 9; i++)
               {
                  if (!String.IsNullOrEmpty(analysis[i]))
                     swBestModels.Write("{0:F4},", Convert.ToDouble(analysis[i]));
                  else
                     swBestModels.Write(",");
               }
               swBestModels.WriteLine();

               //save shatsubset and t's for best model
               if (bm == 0)
               {
                  sHatTotal = Convert.ToDouble(analysis[3]);
                  SE = Convert.ToDouble(analysis[4]);

                  for (int i = 12; i < 19; i++)
                  {
                     if (analysis[i] != "")
                        T[i - 12] = Convert.ToDouble(analysis[i]);
                  }
               }
            }
            else
               swBestModels.WriteLine();
         }

         //Discounted Model for Best Model
         if (foundAnalysis[0] == 1 && bm == 9)
         {
            //Step down from Four Mixed to Three Mixed
            if (bestModelTau[0, 0] == 5)
            {
               double LCB = 0.0;
               double UCB = 0.0;

               double cStar = DiscountedFourToThreeMixedExponentialModel(bestModelTau[0, 1], T,
                  ref sHatTotal, ref SE, ref LCB, ref UCB);

               swBestModels.WriteLine("{0},ThreeMixedExp,{1},{2:F0},{3:F1},{4:F1},{5:F1},{6:F1}",
                   bestDescription[10], freq[bestModelTau[0, 1]], cStar, sHatTotal, SE, LCB, UCB);
            }

            //Step down from Three Mixed to Two Mixed
            if (bestModelTau[0, 0] == 4)
            {
               double LCB = 0.0;
               double UCB = 0.0;

               double cStar = DiscountedThreeToTwoMixedExponentialModel(bestModelTau[0, 1], T,
                  ref sHatTotal, ref SE, ref LCB, ref UCB);

               swBestModels.WriteLine("{0},TwoMixedExp,{1},{2:F0},{3:F1},{4:F1},{5:F1},{6:F1}",
                   bestDescription[10], freq[bestModelTau[0, 1]], cStar, sHatTotal, SE, LCB, UCB);
            }

            //Discount from Two Mixed to Single Exponential
            if (bestModelTau[0, 0] == 3)
            {
               double LCB = 0.0;
               double UCB = 0.0;

               double cStar = DiscountedTwoToSingleExponentialModel(bestModelTau[0, 1], T,
                  ref sHatTotal, ref SE, ref LCB, ref UCB);

               swBestModels.WriteLine("{0},SingleExp,{1},{2:F0},{3:F1},{4:F1},{5:F1},{6:F1}",
                   bestDescription[10], freq[bestModelTau[0, 1]], cStar, sHatTotal, SE, LCB, UCB);
            }
         }
		}

      //Write out best analysis data for non-parametric models
      swBestModels.WriteLine();
      swBestModels.Write("Total Number of Observed Species = {0},Model,Tau,",
              s[obsMax].ToString());
      swBestModels.WriteLine("Observed Sp,Estimated Total Sp,SE,Lower CB,Upper CB,CV_rare");

      for (int bm = 5; bm < 10; bm++)
      {
         if (bm == 5 || bm == 6 || bm == 9)
         {

            swBestModels.Write("{0},", bestDescription[bm]);

            if (foundAnalysis[bm] == 1)
            {
               string[] analysis = bestAnalysis[bm].Split(',');

               //Chao--make tau = 2 (all are the same)
               if (bm == 5)
                  analysis[1] = "2";

               for (int i = 0; i <= 2; i++)
                  swBestModels.Write("{0},", analysis[i]);

               for (int i = 3; i <= 6; i++)
               {
                  if (!String.IsNullOrEmpty(analysis[i]))
                     swBestModels.Write("{0:F1},", Convert.ToDouble(analysis[i]));
                  else
                     swBestModels.Write(",");
               }
               if (bm == 6 || bm == 9)
                  swBestModels.Write("{0:F2},", cvrare);
               swBestModels.WriteLine();
            }
            else
               swBestModels.WriteLine();
         }
      }
      
        

		swBestModels.Close();
	}

	//*********************************************************************
	//**                  Write out fits file for Best Models           ***
	//*********************************************************************
	public void WriteBestModelsFitsFile(string outputFileName,
		int[,] bestModelTau, string[] modelDescription)
	{
		StreamWriter swBestModels = new StreamWriter(outputFileName +
			"_BestModelsFits.csv");

		//Find the largest tau of the 4 models
		int biggestTau = 0;
		for (int bm = 0; bm < 5; bm++)
			if (bestModelTau[bm, 1] > biggestTau)
				biggestTau = bestModelTau[bm, 1];

		//Write column headers
		swBestModels.Write("Frequency,Observed,");
		swBestModels.Write("Best--{0}/Tau {1},",
			modelDescription[bestModelTau[0, 0]], freq[bestModelTau[0, 1]]);
		swBestModels.Write("Other 1--{0}/Tau {1},",
			modelDescription[bestModelTau[1, 0]], freq[bestModelTau[1, 1]]);
		swBestModels.Write("Other 2--{0}/Tau {1},",
			modelDescription[bestModelTau[2, 0]], freq[bestModelTau[2, 1]]);
		swBestModels.Write("Other 3--{0}/Tau {1},",
			modelDescription[bestModelTau[3, 0]], freq[bestModelTau[3, 1]]);

      //strip off WLR from description
      string temp = modelDescription[bestModelTau[4, 0]].
         Remove(modelDescription[bestModelTau[4, 0]].Length - 3);

      swBestModels.WriteLine("WLRM--{0}/Tau {1}",
         temp, freq[bestModelTau[4, 1]]);

		double[,] calculatedFits = new double [5, freq[biggestTau] + 1];

		for (int bm = 0; bm < 5; bm++)
		{
			if (bestModelTau[bm, 0] > 0)
			{
				int r = bestModelTau[bm, 1];
				double[] fitsCount = new double[freq[r] + 1];
				int fitsCheck = 1;

				//Poisson
				if (bestModelTau[bm, 0] == 1)
				{					
					double mlesPoisson = 0.0;
					double mlesPoissonExp = 0.0;
					double lnFactorial = 0.0;

					PoissonFits(r, ref mlesPoisson, ref mlesPoissonExp, 
						ref fitsCheck, ref lnFactorial, ref fitsCount);
				}

				//Single Exponential
				else if (bestModelTau[bm, 0] == 2)
				{
					double mlesSExp = 0.0;
	
					SingleExponentialFits(r, ref mlesSExp, ref fitsCheck,
						ref fitsCount);
				}

				//Two Mixed Exponential
				else if (bestModelTau[bm, 0] == 3)
				{
					double u = 0.0; ;
					double mlesSExp1 = 0.0;
					double mlesSExp2 = 0.0;
					double mlesSExp3 = 0.0;

					TwoMixedExponentialFits(r, ref u, ref mlesSExp1,
						ref mlesSExp2, ref mlesSExp3, ref fitsCheck, ref  fitsCount);
				}

				//Three Mixed Exponential
				else if (bestModelTau[bm, 0] == 4)
				{
					double u1 = 0.0;
					double u2 = 0.0;
					double mlesSExp1 = 0.0;
					double mlesSExp2 = 0.0;
					double mlesSExp3 = 0.0;
					double mlesSExp4 = 0.0;
					double mlesSExp5 = 0.0;

					ThreeMixedExponentialFits(r, ref u1, ref u2,
						ref  mlesSExp1, ref  mlesSExp2, ref mlesSExp3, ref mlesSExp4,
						ref  mlesSExp5, ref  fitsCheck, ref fitsCount);
				}

                //Four Mixed Exponential
                else if (bestModelTau[bm, 0] == 5)
                {
                    double u1 = 0.0;
                    double u2 = 0.0;
                    double u3 = 0.0;
                    double mlesSExp1 = 0.0;
                    double mlesSExp2 = 0.0;
                    double mlesSExp3 = 0.0;
                    double mlesSExp4 = 0.0;
                    double mlesSExp5 = 0.0;
                    double mlesSExp6 = 0.0;
                    double mlesSExp7 = 0.0;

                    FourMixedExponentialFits(r, ref u1, ref u2, ref u3,
                        ref  mlesSExp1, ref  mlesSExp2, ref mlesSExp3, ref mlesSExp4,
                        ref  mlesSExp5, ref  mlesSExp6, ref  mlesSExp7, ref  fitsCheck,
                        ref fitsCount);
                }

                 //weighted linear regression is alway other 4
                else if (bestModelTau[bm, 0] == 6)
                {
                    double gamma = 0.0;
                    double delta = 0.0;
                    double MSE = 0.0;
                    double k = 0.0;
                    LogTWLRFits(r, ref gamma, ref delta, ref MSE, ref k,
                        ref fitsCheck, ref fitsCount);
                }

                //weighted linear regression is alway other 4
                else if (bestModelTau[bm, 0] == 7)
                {
                    double gamma = 0.0;
                    double delta = 0.0;
                    double MSE = 0.0;
                    double k = 0.0;
                    WLRFits(r, ref gamma, ref delta, ref MSE, ref k,
                        ref fitsCheck, ref fitsCount);
                }
				
				for (int t = 1; t <= freq[r]; t++)
					calculatedFits[bm, t] = fitsCount[t];
			}
		}

		//Write out fits data--non zero frequencies
		int rr = 1;
		for (int t = 1; t <= freq[biggestTau]; t++)
		{
			if (t == freq[rr])
			{
				swBestModels.Write("{0},{1}", t, observedCount[rr]);

				for (int bm = 0; bm < 5; bm++)
				{
					if (t <= freq[bestModelTau[bm, 1]])
						swBestModels.Write(",{0}", calculatedFits[bm, t]);
					else
						swBestModels.Write(",");
				}
				swBestModels.WriteLine();
				rr++;
			}
		}
		swBestModels.Close();
	}

	//*********************************************************************
	//**             Write out file for Bubble Plots                    ***
	//*********************************************************************
	public void WriteBubblePlotFile(string outputFileName)
	{
		StreamWriter swBubblePlot = new StreamWriter(outputFileName +
			"_BubblePlot.csv");

		//Determine plot order
		string[] modelBubbleDescription = {"None", "Poisson", "SingleExp", 
                      "TwoMixedExp", "ThreeMixedExp", "FourMixedExp",
                      "WLRM", "NonParametric"};

		// default order
		int[] plotorder = { 1, 2, 3, 4, 5, 6 };
		double[] minSE = new double[6];

		//first is alway nonparametric (7), so don't calculate
		for (int m = 1; m < 7; m++)
		{
			if (bubblePlotDataFlag[m, freqTau10] == 1)
			{
				string[] parts = new string[2];
				parts = bubblePlotData[m, freqTau10].Split(',');
				minSE[m - 1] = Convert.ToDouble(parts[1]);
			}
			else
				minSE[m - 1] = 1000000000.00;
		}

		Array.Sort(minSE, plotorder);

		swBubblePlot.Write("Tau,Observed Sp,");
		swBubblePlot.Write("Est Sp for {0} Model,SE for {0} Model,",
			 modelBubbleDescription[7]);

		for (int p = 5; p > 0; p--)
			swBubblePlot.Write("Est Sp for {0} Model,SE for {0} Model,",
				 modelBubbleDescription[plotorder[p]]);

		swBubblePlot.WriteLine("Est Sp for {0} Model,SE for {0} Model,",
				  modelBubbleDescription[plotorder[0]]);

		for (int r = freqTau10; r <= obsMax; r++)
		{
			swBubblePlot.Write("{0},{1},", freq[r], s[r]);

			//do non-parametrics first
			int m = 7;
			if (bubblePlotDataFlag[m, r] == 1)
			{
				//Apply more filters for better graphs
				string[] parts = new string[2];
				parts = bubblePlotData[m, r].Split(',');
				double num = Convert.ToDouble(parts[0]);
				double se = Convert.ToDouble(parts[1]);
				if ((num < (100 * s[r])) && (se < (10 * s[r])))
					swBubblePlot.Write("{0},", bubblePlotData[m, r]);
				else
					swBubblePlot.Write(",,");
			}
			else
				swBubblePlot.Write(",,");

			//if there is data, write it otherwise leave blank
			for (int p = 5; p >= 0; p--)
			{
				m = plotorder[p];
				if (bubblePlotDataFlag[m, r] == 1)
				{
					//Apply more filters for better graphs
					string[] parts = new string[2];
					parts = bubblePlotData[m, r].Split(',');
					double num = Convert.ToDouble(parts[0]);
					double se = Convert.ToDouble(parts[1]);
					if ((num < (100 * s[r])) && (se < (10 * s[r])))
						swBubblePlot.Write("{0},", bubblePlotData[m, r]);
					else
						swBubblePlot.Write(",,");
				}
				else
					swBubblePlot.Write(",,");
			}
			swBubblePlot.WriteLine();
		}
		swBubblePlot.Close();
	}
}
