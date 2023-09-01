function ProxBaseSocNetCode
%%% This M code was composed by ORR SPIEGEL JAN 2016 for Methods In Ecology and Evolution%%%%%
%This function creates the proximity network for data loaded from files:
%simulated data /the lizards data  / user's data 
%It creates the dataset randomization and generates the plots and stats
%Sim is for simulated, Obs is for Observed data, Sf is for shaffled (randomized) data. 
%calls helper functions NetworkCalc5 FigMakerLizNtWrkRand1

%% parameters for user to change
MaxInrtactDist=14;%meters to consider as an interaction among individuals
make_suffeles=2;%do i want to generate random shuffles? 0- no shuffling, 1 - shuffeling over the whole season 2 shuffeling over a limited time only
shuffles=100;% if yes (make_suffeles is 1 or 2), how many times to shuffle dates for random networks
DaysTolerance=[125 100 75 50 25 10 5 3 2];% the time window for shufling dates for make_suffeles=2. if make_suffeles==1 then it will use only the first value
Window=2; %window size in minutes around the time to be considered as same time (2*window)
SamplingInterval=10;% the typical sampling interval in minutes

%% asking the user what data to work on?
% Construct a questdlg with three options
choice = questdlg('What data do you want to work on?', ...
	'Menu', 'lizards','simulations','user data','lizards');
% Handle response
switch choice
    case 'lizards'; DataSource= 1;
    case 'simulations'; DataSource= 2;
    case 'user data'; DataSource= 3;
end

%% Setting empty vars and time steps and some under-the hood place holder
DataOverLap=struct();%number of cases both individuals has GPS
IntractCntr=struct(); %counting interactions by dyads (for the different threshorlds)
DailyTimeSpan=struct();%temporal ranges
IndvNames=struct(); %list of individuals tracked in the dataset 
IndvSex=struct(); %list of sex of these individuals 
HR_Cntr=struct(); %center of HR
SpatProxList=struct();%a full list of time and distance of all collectd interactinos 
ObsSRI=struct();%simple ratio index= num of interaction / num of both active (Leu et al 2010)
ObsDegree=struct();%how many different individuals connected to this one
ObsStrength=struct();%sum of SRIs (==edge weights for this lizard).strength- from bull 2012 mol ecol. mean strength... mean strength =the average edge weight 
ObsStrengthCorrct=struct();%same as strength but vedided by sum of strength to allow comparisons
ObsMnEdgWeight=struct();%mean edge weight for each individual
ObsNtWk_Density=struct(); %Strength/number of possible links (n-1)? from Godfrey et al. / Animal Behaviour 86 (2013) 763e772

indxOfDefltinList=1;
if make_suffeles==1;DaysTolerance=DaysTolerance(1);end% using the first value for a single time frame (make_suffeles==1)
OneMin=datenum('00:01:00','HH:MM:SS')-datenum('00:00:00','HH:MM:SS');

%% loading simulated/observed/user data and reading it into paremeters
if DataSource==1
    load('Lizards_Data2010.mat')
    FieldName='Lizards';
    
elseif DataSource==2
    load('xyFromSimulationForSNanalysis.mat')
    %XY- a stucture with these field: indiv,step,Day,StepInDay,burst,x,y,pseudoSex
    %HRCntsXY- a matrix of the coordinates of the HR center with 3 colums: X, Y, the sex of the individual (1 male, 2 female) 
    UGPSname =cellfun(@str2num, XY.indiv);%SimIndv 60 individuals   
    SimStep= double(XY.step);%this was 5000 steps in simualted data 50*100 days
    UGPSDateAsNum=double(XY.Day);%100 days SimDay
    SimStepInDay=double(XY.StepInDay);%now 50 steps in each day
    %translating steps to minutes with a chosen time intervals
    UGPSTimeAsNum=SimStepInDay*SamplingInterval*OneMin;
    SimXY=[XY.x,XY.y];%XY DATA 
    UUTM_Easting=SimXY(:,1);UUTM_Northing=SimXY(:,2);
    ULizSex= XY.pseudoSex; %SimPseudo=SexHRCntXY(:,3)
    clear XY   
    
    NofSimIndv=length(unique(UGPSname));
    FixesForSimIndiv=length(unique(SimStep));%steps perDay is  length(unique(SimStepInDay))
    FieldName='Simulated';

elseif DataSource==3
    %reading user data xls and setting the same names
    UserData = readtable('UserData.xlsx');
    UGPSname=UserData.Individual;
    UGPSDateAsNum=datenum(datetime(UserData.Date, 'ConvertFrom','excel'));
    UGPSTimeAsNum=UserData.Time;
    UUTM_Easting=UserData.X;
    UUTM_Northing=UserData.Y;
    ULizSex=UserData.Sex;
   
    FieldName='UserData';clear UserData 
end
disp(['working with ',FieldName, ' data'])

%% Setting Field names    
DailyTimeSpan.(FieldName)=[]; %first point, last point, Number of 10 minutes gaps
DataOverLap.(FieldName)=[];    IntractCntr.(FieldName)=[];    ObsDegree.(FieldName)=[];     
ObsStrength.(FieldName)=[];    ObsStrengthCorrct.(FieldName)=[];    
ObsMnEdgWeight.(FieldName)=[];    ObsNtWk_Density.(FieldName)=[];    
SpatProxList.(FieldName)=[];     
    
IndvNames.(FieldName)=unique(UGPSname);  
for lzS=1:length(IndvNames.(FieldName))%loop on lizards to log sex
    IndvSex.(FieldName)(lzS)=ULizSex(find(UGPSname==IndvNames.(FieldName)(lzS),1,'first'));
    indx1=(UGPSname==IndvNames.(FieldName)(lzS) );
    HR_Cntr.(FieldName)(lzS,1:2)=[round(nanmean(UUTM_Easting(indx1))),round(nanmean(UUTM_Northing(indx1)))];
    clear indx1
end

DaysInSeason=min(UGPSDateAsNum):max(UGPSDateAsNum);%sequence of dates with Data  

%% sending Data to NetworkCalc5 for loop on days and timesteps    (check if not Networkcalc4b!) 
[DataOverLap.(FieldName),DailyTimeSpan.(FieldName),IntractCntr.(FieldName),...
    SpatProxList.(FieldName), ObsSRI.(FieldName),ObsStrength.(FieldName),...
    ObsNtWk_Density.(FieldName), ObsMnEdgWeight.(FieldName),ObsDegree.(FieldName)]=...
NetworkCalc5(DaysInSeason,UGPSDateAsNum,UGPSTimeAsNum,...
UGPSname,IndvNames.(FieldName),UUTM_Easting,UUTM_Northing,...
MaxInrtactDist,Window,IndvSex.(FieldName),SamplingInterval);
%each row is a male, each colum is a female: malesNames=IndvNames.Y_2010(IndvSex.Y_2010==1)
    
%% shuffles %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% make shuffles for Data - the whole period only, (not for different intervals) 
if make_suffeles==1
   %DaysTolerance=125;% there are 125 days of data in 2009 and 115 in 2010, 100 in the simulated  
   %% making parameters to store values for the shuffled networks  
   % preparing parameters for this set. the third dimention is to be
   % competible with the next part where i have different wdaytol windows
   DayTolSfDegree.(FieldName)=nan(length(IndvNames.(FieldName)),shuffles,1);
   DayTolSfStrength.(FieldName)=DayTolSfDegree.(FieldName);

   SfDailyTimeSpan.(FieldName)=[];     SfDataOverLap.(FieldName)=[];    SfIntractCntr.(FieldName)=[];    SfDegree.(FieldName)=[];
   SfStrength.(FieldName)=[]; SfSRI.(FieldName)=[];  SfMnEdgWeight.(FieldName)=[];    SfNtWk_Density.(FieldName)=[];    SfSpatProxList=[];

   SuffledDates=nan(length(UGPSDateAsNum),shuffles);
   disp('working on shuffled network for the whole period');
     
   % random days colum for each lizard  
   
   %% loop on lizards to find and shuffle unique days for this season
   for lzS=1:length(IndvNames.(FieldName))%loop on lizards to log sex
      indx1=(UGPSname==IndvNames.(FieldName)(lzS) );
      DateLizYear=unique(UGPSDateAsNum(indx1));
      for ss=1:shuffles%loop on different shuffels
          SuffeledList=DateLizYear(randperm(length(DateLizYear)));
          %just to make sure iit is identical:   SuffeledList=sort(DateLizYear(randperm(length(DateLizYear))));

          %now setting the new dates 
          for sd=1:length(DateLizYear) %loop on days of this lizard - to shuffel
             indx1= (UGPSname==IndvNames.(FieldName)(lzS) & UGPSDateAsNum==DateLizYear(sd));
             SuffledDates(indx1,ss)=SuffeledList(sd);
          end%loop on days of this lizard - to shuffel
      end%loop on different shuffels
  end %loop on lizards        

   %% sending suffelled data to NetworkCalc5 in a loop of suffles  - check if not  Networkcalc4b         
   for ss=1:shuffles%loop on different shuffels
    [SfDataOverLap.(FieldName)(:,:,ss),SfDailyTimeSpan.(FieldName),SfIntractCntr.(FieldName)(:,:,ss),SfSpatProxList.(FieldName), SfSRI.(FieldName)(:,:,ss),SfStrength.(FieldName)(:,ss),...
    SfNtWk_Density.(FieldName)(ss),  SfMnEdgWeight.(FieldName)(:,ss),SfDegree.(FieldName)(:,ss) ]=...
    NetworkCalc5(DaysInSeason,SuffledDates(:,ss),UGPSTimeAsNum,...
    UGPSname,IndvNames.(FieldName),UUTM_Easting,UUTM_Northing,...
    MaxInrtactDist,Window,IndvSex.(FieldName),SamplingInterval);

    disp(['done with shuffle ',num2str(ss),' out of ',num2str(shuffles),' shuffles for observed',FieldName,' data'])
   end%loop on shuffles
  
   %% storing the DayTolerance Effects
    DayTolSfDegree.(FieldName)(:,:,1)=SfDegree.(FieldName);
    DayTolSfStrength.(FieldName)(:,:,1)=SfStrength.(FieldName);

end%make shuffels? make_suffeles==1  

%% make shuffles for a given time window for observed network   
if make_suffeles==2
   disp('working on different time windows');
   %% making parameters to store values for different DaysTolerance values
   DayTolSfDegree.(FieldName)=nan(length(IndvNames.(FieldName)),shuffles,length(DaysTolerance));
   DayTolSfStrength.(FieldName)=DayTolSfDegree.(FieldName);
   
   %% loop on DaysTolerance values for observed data
   for DayTolCntr=1:length(DaysTolerance)%loop on different values of (DayTolCntr)
       disp(['now working on ', num2str(DaysTolerance(DayTolCntr)),' the',num2str(DayTolCntr),' value out of ',num2str(length(DaysTolerance))])
       %% preparing parameters for this year:       
       SfDailyTimeSpan.(FieldName)=[];     SfDataOverLap.(FieldName)=[];    SfIntractCntr.(FieldName)=[];    SfDegree.(FieldName)=[];
       SfStrength.(FieldName)=[]; SfMnEdgWeight.(FieldName)=[];    SfNtWk_Density.(FieldName)=[];    SfSpatProxList=[];SfSRI.(FieldName)=[];

       %% loop on lizards to find and shuffle unique days for this season
       for ss=1:shuffles%loop on different shuffels
        % random days colum for each lizard 
        SuffledDates=nan(length(UGPSDateAsNum),1);
        for lzS=1:length(IndvNames.(FieldName))%loop on lizards to log sex
            indx1=(UGPSname==IndvNames.(FieldName)(lzS));
            DateLizYear=unique(UGPSDateAsNum(indx1));
            %for ss=1:shuffles%loop on different shuffels                
            SuffeledList=[];%start an empty list
            for dayswindow=min(DateLizYear):DaysTolerance(DayTolCntr): (max(DateLizYear)+DaysTolerance(DayTolCntr))
                dayToshuffle=(DateLizYear(DateLizYear>=dayswindow & DateLizYear<dayswindow+DaysTolerance(DayTolCntr)));%only days with in this range will be shuffled
                if ~isempty(dayToshuffle)%do i have days to shuffle in this current DaysTolerance interval
                    SuffeledList=[SuffeledList;dayToshuffle(randperm(length(dayToshuffle)))];%#ok<AGROW> %add this block to the list
                end%do i have days to shuffle in this current DaysTolerance interval
            end%loop on days in this step for the current DaysTolerance

            %now setting the new dates 
            for sd=1:length(DateLizYear) %loop on days of this lizard - to shuffel
                indx1= (UGPSname==IndvNames.(FieldName)(lzS) & UGPSDateAsNum==DateLizYear(sd));
                SuffledDates(indx1,1)=SuffeledList(sd);
            end%loop on days of this lizard - to shuffel
        end %loop on lizards
      
       %% sending suffelled data to NetworkCalc5 in a loop of suffles -check if not  Networkcalc4b 
        [SfDataOverLap.(FieldName)(:,:,ss),SfDailyTimeSpan.(FieldName),SfIntractCntr.(FieldName)(:,:,ss),SfSpatProxList.(FieldName), SfSRI.(FieldName)(:,:,ss),SfStrength.(FieldName)(:,ss),...
        SfNtWk_Density.(FieldName)(ss),SfMnEdgWeight.(FieldName)(:,ss),SfDegree.(FieldName)(:,ss)]=...
        NetworkCalc5(DaysInSeason,SuffledDates(:,1),UGPSTimeAsNum,...check if not  Networkcalc4b 
        UGPSname,IndvNames.(FieldName),UUTM_Easting,UUTM_Northing,...
        MaxInrtactDist,Window,IndvSex.(FieldName),SamplingInterval);    
       end%loop on shuffles       
       
       %% storing the DayTolerance Effects
        % preparing parameters for this year:
        DayTolSfDegree.(FieldName)(:,:,DayTolCntr)=SfDegree.(FieldName);
        DayTolSfStrength.(FieldName)(:,:,DayTolCntr)=SfStrength.(FieldName);         
   end %loop on values of DaysTolerance for the observed network
end%make shuffels for a changing time window for observed netwokr?

%% Saving data after the loops
clear DataSource SuffledDates years DateLizYear dayswindow DayTolCntr dayToshuffle indx1 lzS sd ss SuffeledList      
save (['ProxBaseSocNetCodeWith',num2str(make_suffeles),'Nshuf',num2str(shuffles),'DayTolvalues',num2str(length(DaysTolerance)),FieldName,'Data.mat']);

%% plots and stats %%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%% descriptive stats of the PBSN how many interactnios / degrees per individual
disp(['total number of assosiations ',num2str(sum(sum(IntractCntr.(FieldName))))])%this count each interactnios twice (since both individuals has it)
disp(['Mean number of assosiations per individual ',num2str(mean(sum(IntractCntr.(FieldName))))])
disp(['STD of number of assosiations per individual ',num2str(std(sum(IntractCntr.(FieldName))))])
       
disp(['Mean degree for individual ',num2str(mean(ObsDegree.(FieldName)))]) 
disp(['STD of degree among individual ',num2str(std(ObsDegree.(FieldName)))]) 

disp(['Mean Strength for individual ',num2str(mean(ObsStrength.(FieldName)))]) 
disp(['STD of Strength among individual ',num2str(std(ObsStrength.(FieldName)))]) 

%% figures for degrees and Strength
%degree by indivdiuals, sorted by observed
FigMakerLizNtWrkRand1(1,ObsDegree.(FieldName)(:,indxOfDefltinList),DayTolSfDegree.(FieldName),'Degree',DaysTolerance,FieldName,shuffles)
%degree difference ofthe network by shufling period
if make_suffeles==2;FigMakerLizNtWrkRand1(3,ObsDegree.(FieldName)(:,indxOfDefltinList),DayTolSfDegree.(FieldName),'Degree',DaysTolerance,FieldName,shuffles);end

%Strength by indivdiuals, sorted by observed
FigMakerLizNtWrkRand1(1,ObsStrength.(FieldName)(:,indxOfDefltinList),DayTolSfStrength.(FieldName),'Strength',DaysTolerance,FieldName,shuffles)
%Strength difference ofthe network by shufling period
if make_suffeles==2;FigMakerLizNtWrkRand1(3,ObsStrength.(FieldName)(:,indxOfDefltinList),DayTolSfStrength.(FieldName),'Strength',DaysTolerance,FieldName,shuffles);end

%% SN Graph: edge strengh (line width) and node degree (node size) 
% this figure is based on a code  (by Mike Wu) can be downloaded from Mathwork. michael.wu@lithium.com (May 2009)
%http://www.mathworks.com/matlabcentral/fileexchange/24035-wgplot-weighted-graph-plot--a-better-version-of-gplot-

Aa=ObsSRI.(FieldName)(: , : ,indxOfDefltinList); Aa=sparse(Aa);
figure;[he,hv]=wgPlot(Aa,HR_Cntr.(FieldName),...%matrix of edges + where to put each lizard
    'vertexWeight',ObsDegree.(FieldName)(:,indxOfDefltinList),...%nodes size
     'vertexScale',200,'edgeWidth',2,...
     'vertexMetadata',IndvSex.(FieldName) ,'vertexColorMap',cool,...nodes color map
     'edgeColorMap',jet);%edge color map
title(['SN Graph for ',FieldName])

end%of function








