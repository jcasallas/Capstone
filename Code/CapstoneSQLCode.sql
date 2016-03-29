
/*Creating Installation counts by Zip*/

SELECT COUNT([Zip Code]) AS InstallationsPerZip, [Zip Code]  INTO #InstallationsByZip FROM [JC_NY_SolarInstall_Capstone]  group by [Zip Code]

/*Data holding table with zip data and from Census data available at Factfinder.com*/

SELECT DISTINCT 
	 
	CAP.Zip
,	InstallationsPerZip
,	US.State
,	Cap.pop/3 AS HouseHolds
,	Cap.pop
,	CAp.Mean AS AverageIncome
,	CASE WHEN [Average HH Size] <>'-' then [Average HH Size]
	ELSE (SELECT AVG (CAST([Average HH Size] AS float))  FROM [JC_Zip_CapPopIncome] where [Average HH Size] <>'-')
	END AS [Average HH Size]
, [OwnerOccupancyPercentage]
, [OwnerDetachedpercentage]

INTO JC_NY_InstallsPerZip_CP
FROM [dbo].[JC_Zip_CapPopIncome] AS Cap
INNER JOIN [dbo].[JC_CAP_OwnerRenter_Units] AS Own
ON Own.Zip = Cap.Zip

INNER JOIN [dbo].[JC_ZipDataUS] AS US
ON US.Zip= CAP.Zip
LEFT JOIN #InstallationsByZip AS InstallsZ
ON InstallsZ.[Zip Code] =CAp.Zip

LEFT JOIN [dbo].[JC_NY_SolarInstall_Capstone] AS Installs
ON Installs.[Zip Code]= CAp.Zip
WHERE US.state='NY'





/******Adding Solar Installation Data from NY open data site ********/


SELECT 

SolarIns.Zip
,CAP.City	
,CAP.County	
,CAP.State	
,CAP.Sector	
,CAP.[Electric Utility]
,CAP.[Expected KWh Annual Production] 
,CAST(SolarIns.InstallationsPerZip	as int) AS InstallationsPerZip
,CAST (SolarIns.HouseHolds	as int) AS HouseHolds
--,CAST(SolarIns.InstallationsPerZip	as int)/CAST (SolarIns.HouseHolds	as int) as SolarPenetration
,SolarIns.pop	
,SolarIns.AverageIncome
,CAST (SolarIns.[Average HH Size] as float) AS [Average HH Size]
, [OwnerOccupancyPercentage]
, [OwnerDetachedpercentage]

INTO  #JC_NY_SolarCap_2016
FROM JC_NY_InstallsPerZip_CP AS SolarIns

LEFT JOIN  [dbo].[JC_NY_SolarInstall_Capstone]AS  CAP
ON Cap.[Zip Code]= SolarIns.Zip

SELECT * FROM #JC_NY_SolarCap_2016

/******************************/
--Dealing with low households and Missing data
/******************************/
UPDATE #JC_NY_SolarCap_2016
SET HouseHolds = pop
WHERE HouseHolds <1


UPDATE #JC_NY_SolarCap_2016
SET [Electric Utility]='Other'
WHERE [Electric Utility] IS NULL

UPDATE #JC_NY_SolarCap_2016
SET InstallationsPerZip = 0
WHERE InstallationsPerZip IS NULL


/******************************/



SELECT 

	Zip
,City	
,County	
,State	
,Sector	
,[Electric Utility]
,[Expected KWh Annual Production] 
,CAST(InstallationsPerZip	as int) AS InstallationsPerZip
,CAST (HouseHolds	as int) AS HouseHolds
,CAST(CAST(InstallationsPerZip	as float)/CAST (HouseHolds	as float) AS FLOAT )as SolarPenetration
,pop	
,AverageIncome
,CAST ([Average HH Size] as float) AS [Average HH Size]
, [OwnerOccupancyPercentage]
, [OwnerDetachedpercentage]

INTO   JC_NY_SolarCap_2016
FROM #JC_NY_SolarCap_2016 




/******FinalTable*******/

 SELECT DISTINCT 
	Zip
 ,	InstallationsPerZip
 ,	[Households]
 ,	[Pop]
 ,	[AverageIncome]
 ,	NTILE(10) OVER(ORDER BY  [AverageIncome] DESC) AS Centile_HHI
 ,	SolarPenetration AS Sola_Penetration
 ,	NTILE(10) OVER(ORDER BY  SolarPenetration DESC) AS Centile_Solar_Penetration
 ,  CASE WHEN InstallationsPerZip >0 THEN '1' ELSE '0' END  AS SolarInstalled
 ,	[Average HH Size]
 ,	NTILE(10) OVER(ORDER BY  [Average HH Size] DESC) AS Centile_HHSize
,	[Electric Utility]
, [OwnerOccupancyPercentage]
, [OwnerDetachedpercentage]
  
FROM  JC_NY_SolarCap_2016
 

