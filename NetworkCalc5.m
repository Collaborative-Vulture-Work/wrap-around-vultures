function [DataOverLap,DailyTimeSpan,IntractCntr,SpatProxList, SRI,Strength,NtWk_Density,MnEdgWeit,Degree]=...
    NetworkCalc5(DaysInSeason,DateNum,UGPSTimeAsNum,UGPSname,LizNamesCurrYear,UUTM_Easting,UUTM_Northing,...
    MaxInrtactDist,Window, IndivSex,SamplingInterval)
%this function is called by ProxBaseSocNetCode, gets the empty vars and fills them with a network structute


%% preparing vars
OneMin=datenum('00:01:00','HH:MM:SS')-datenum('00:00:00','HH:MM:SS');
WindowM=(Window*OneMin);% expressing the window size as matlab number
d=0;%day counter

DailyTimeSpan=zeros(length(DaysInSeason),4); %first point, last point, Number of 10 minutes gaps   
DataOverLap=zeros(length(LizNamesCurrYear),length(LizNamesCurrYear));
IntractCntr=zeros(length(LizNamesCurrYear),length(LizNamesCurrYear),length(MaxInrtactDist));    
Degree=zeros(length(LizNamesCurrYear),length(MaxInrtactDist));
Strength=Degree;
MnEdgWeit=Degree;
NtWk_Density=zeros(1,length(MaxInrtactDist));
SpatProxList=[];
    
%% loop on days in this season
    for CurrntDay=DaysInSeason(1):DaysInSeason(end)
        d=d+1 ;      
        CurDayInd=find(DateNum==CurrntDay);
        if ~isempty(CurDayInd) % if 100% of indvi have Sync missing points this day can be missing from the dataset
            firstPoint=datevec(min(UGPSTimeAsNum(CurDayInd)));
            firstPoint(5)=floor(firstPoint(5));firstPoint(6)=0;%to the nearest earlier minute, seconds to zeros
            lastPoint=datevec(max(UGPSTimeAsNum(CurDayInd))); lastPoint(5) =ceil(lastPoint(5));lastPoint(6)=59;%to the neares later 10 min, seconds to 60
            TodaysTimeSteps=datenum(firstPoint):(OneMin*SamplingInterval):datenum(lastPoint);
            DailyTimeSpan(d,:)=[CurrntDay, datenum(firstPoint),datenum(lastPoint),round(datenum(lastPoint)- datenum(firstPoint))/(OneMin*SamplingInterval)];%first point, last point, Number of 10 minutes gaps 

            %% loop on all points of the day
            for tpointCntr=1:length(TodaysTimeSteps)
                %disp(['day:',num2str(d),' time point',num2str(tpointCntr)]);
                CurrTimePoint=TodaysTimeSteps(tpointCntr);            
                CurTimeInd=CurDayInd(UGPSTimeAsNum(CurDayInd)>=(CurrTimePoint-WindowM) &  UGPSTimeAsNum(CurDayInd)<=(CurrTimePoint+WindowM));%all points with data within the time window and date and year
                if ~isempty(CurTimeInd)%do i have data in this time point
                    LizThisTime= unique(UGPSname(CurTimeInd));
                    %disp(['LizThisTime: ',num2str(length(LizThisTime))]);
                    if length(LizThisTime)>1%more than one lizards with Data
                        [~,LizIndxInList]= ismember(LizThisTime,LizNamesCurrYear);
                        %disp(length(LizIndxInList));
                        PairsLizInd=FindPairs(LizIndxInList);%sending the indixes of the lizards to external funciton for finding pairs
                        %PairsLizInd=[PairsLizInd;PairsLizInd(:,2),PairsLizInd(:,1)];%#ok<AGROW> %both sized of the diagonal of the matrix
                        Ind1InMtrx=[PairsLizInd(:,1);PairsLizInd(:,2)];Ind2InMtrx=[PairsLizInd(:,2);PairsLizInd(:,1)];%both sized of the diagonal of the matrix
                        for ii=1:length(Ind1InMtrx)
                            DataOverLap(Ind1InMtrx(ii),Ind2InMtrx(ii))=DataOverLap(Ind1InMtrx(ii),Ind2InMtrx(ii))+ 1;%adding to the overlap matrix
                        end

                        %% interactions within distance
                        PairsPointInd=FindPairs(CurTimeInd);%sending the indixes of points with Data
                        CurrPairDists=(  (UUTM_Easting(PairsPointInd(:,1)) -UUTM_Easting(PairsPointInd(:,2)) ).^2 +... 
                                         (UUTM_Northing(PairsPointInd(:,1))-UUTM_Northing(PairsPointInd(:,2))).^2  ).^0.5;%distances between pairs of points
                        for DistThresh=1:length(MaxInrtactDist)%loop on the differet values of MaxInrtactDist
                          WithinDistancePairs=find(CurrPairDists<=MaxInrtactDist(DistThresh)); %what pairs are within permitted distance?
                          if ~isempty(WithinDistancePairs)%are there any suitable pairs of locations?
                               PairsPointInd2=PairsPointInd(WithinDistancePairs,:);%the indices of locations within distance.
                               PairsPointInd2=PairsPointInd2((UGPSname(PairsPointInd2(:,1))~= UGPSname(PairsPointInd2(:,2))),:);%making sure it is not the same lizard in the two points...
                                if ~isempty(PairsPointInd2)
                                   [~,Liz1IndxInList]= ismember(UGPSname(PairsPointInd2(:,1)),LizNamesCurrYear);%finding the index in the matrix for each lizard
                                   [~,Liz2IndxInList]= ismember(UGPSname(PairsPointInd2(:,2)),LizNamesCurrYear);%finding the index in the matrix for each lizard
                                   Ind1InMtrx=([Liz1IndxInList;Liz2IndxInList]);Ind2InMtrx=([Liz2IndxInList;Liz1IndxInList]);
                                   SpatProxList=[SpatProxList; repmat(MaxInrtactDist(DistThresh),size(PairsPointInd2,1),1),PairsPointInd2];%this is the list of all interaction: the threshold used, the indices in the lizards data, the times, and their names
                                   for ii=1:length(Ind1InMtrx)%updating new values by cells
                                        IntractCntr(Ind1InMtrx(ii),Ind2InMtrx(ii),DistThresh)=IntractCntr(Ind1InMtrx(ii),Ind2InMtrx(ii),DistThresh)+1;%adding to the overlap matrix
                                   end
                                end%not empty list of PairsPointInd2
                          end%are there any suitable pairs of locations?
                        end%loop on the differet values of MaxInrtactDist
                        
                    end%more than one lizards with Data
                end%do i have data
            end%loop on time points

        end%do i have data for this day?        
    end%loop on all days in this season
   
    %% proccessing year's data for this network
    for DistThresh=1:length(MaxInrtactDist)%loop on the differet values of MaxInrtactDist
        SRI(:,:,DistThresh)= IntractCntr(:,:,DistThresh)./ DataOverLap;  
        SRI2=SRI;SRI2(SRI2==0)=NaN;
        %Strength1(:,DistThresh) = nansum(SRI(:,:,DistThresh),2);
        %Strength2(:,DistThresh) = nanmean(SRI2(:,:,DistThresh),2);
        Strength(:,DistThresh) = nanmean(SRI2(:,:,DistThresh),2);
        
        Strength(Strength==0)=NaN;%strength of zero is nan?
        Degree(:,DistThresh) = sum(spones(SRI(:,:,DistThresh))) -1;%one Nan is the reason for -1
       
        MnEdgWeit(:,DistThresh)=Strength(:,DistThresh)./ Degree(:,DistThresh);
        %another way to calc the same value for mean edge weight: 
        %SRI2=SRI;SRI2(SRI2==0)=NaN;MnEdgWeit(:,DistThresh)=nanmean(SRI2(:,:,DistThresh),2)
        %just checking: nanmean(SRI2(:,:,DistThresh),2)- MnEdgWeit
                       
        NtWk_Density(:,DistThresh)= nansum(nansum(SRI(:,:,DistThresh),2))/((length(SRI)-1)^2);
        
        %tt=squeeze(SRI(:,:,DistThresh));tt=tt(:);tt=tt(tt>0& ~isnan(tt));figure; hist(tt);    
    end
    %adding a log of all interactions
    indx=SpatProxList(:,2:3);
    SpatProxList=[SpatProxList,...
        DateNum(indx(:,1))+UGPSTimeAsNum(indx(:,1)),...%time for the first lizards
        DateNum(indx(:,2))+UGPSTimeAsNum(indx(:,2)),...%time for the first lizards
        (  (UUTM_Easting(indx(:,1)) -UUTM_Easting(indx(:,2)) ).^2 +(UUTM_Northing(indx(:,1))-UUTM_Northing(indx(:,2))).^2  ).^0.5,...%distances between pairs of points
        UGPSname(indx(:,1)),UGPSname(indx(:,2))];%id of the two lizards
    clear indx
    %[a b]=find((SRI(:,:,DistThresh))>0.01);[LizNamesCurrYear.Y_2009(a(1:(length(a)/2))) LizNamesCurrYear.Y_2009(b(1:(length(a)/2)))];
end%of main function
    
%%%%%%%%%%%%%%%%%%Helper function %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function Pairs=FindPairs(ListOfVals)
%this function gets a vectors of number and return pairs made of its values
%with each pair appearing once. called by LizProximNetwork
     [p,q] = meshgrid(ListOfVals);
     Pairs = [p(:) q(:),p(:)-q(:)];
     Pairs=sortrows(Pairs,3);Pairs=Pairs(Pairs(:,3)>0,1:2);
     Pairs=sortrows(Pairs,1);
end%of helper function