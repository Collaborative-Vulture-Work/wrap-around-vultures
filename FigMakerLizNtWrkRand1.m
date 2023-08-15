function FigMakerLizNtWrkRand1(Type,obsValues,SfValues,ValuesName,DaysTolerance,FieldName,shuffles)
%this fuction helps ProxBaseSocNetCode: it plots the ranked degrees and
%differences between observed and randomizations
%three types of plots

%% filtering nan in the observed
NonNans=find(~isnan(obsValues));
obsValues=obsValues(NonNans);
SfValues=SfValues(NonNans,:,:);

%% %%%%%observed and sorted at decreasing order
if Type==1 
    [sortedObsVal, indx]=sort(obsValues,'descend');%sorting by observed values from high to low
    meanSf=squeeze((nanmean(SfValues,2)));%finding the average value in the shuffles
    meanSf=meanSf(indx,:);%sorting the shuffles in the same order

    %% Create figure 1 - mean for each individual Vs differet DayTol shufling results
    figure1 = figure;
    axes1 = axes('Parent',figure1,'FontSize',12,'xtick',0:5:60);    % Create axes
    hold(axes1,'all');
    title(['indiv ', ValuesName,' sorted by observed (red) and randomized by different time windows. ',FieldName,' Data n=',num2str(length(obsValues))])
    for col=1:length(DaysTolerance)
        plot(1:length(sortedObsVal),meanSf(:,col),'linewidth',2,'color',[0 col/length(DaysTolerance) 1/col ])
    end
    plot(1:length(sortedObsVal),sortedObsVal,'linewidth',3,'Marker','o','color',[1 0 0]); %'LineWidth',3,
    legend( [strread(num2str(DaysTolerance),'%s');'obs']);
    
    %% stats test for comparing mean vlaues between observe/simulated and shauffled
    %adding boxlplot instead of mean only for the first value of days tolerance
    sortedShfldVal=squeeze(SfValues(:,:,1));sortedShfldVal=sortedShfldVal(indx,:);%the first DaysTolerance value, individuals are sorted accordin to the observved order 
    %tt3=[sortedObsVal, mean(sortedShfldVal,2)];
    DiffValue=sortedObsVal-nanmean(sortedShfldVal,2);
    disp([nanmean(DiffValue);nanstd(DiffValue);nanmin(DiffValue);nanmax(DiffValue)]);
    
    %regression/correlation  with rank or with degree    
    [b,bint,r,rint,stats]= regress(DiffValue,[ones(size(DiffValue)) , (1:length(DiffValue))']); % Removes NaN data
    %stats: in order, the R2 statistic, the F statistic and its p value, and an estimate of the error variance.
    [RHO,PVAL] = corr((1:length(DiffValue))',DiffValue);
    [RHO,PVAL] = corr(sortedObsVal,DiffValue);
    
    [h p ci, stats]=ttest(sortedObsVal,nanmean(sortedShfldVal,2));
    disp(['t test, compare individual observed',num2str(ValuesName),' with mean shuffled one. ttest p: ',num2str(p),' stats: ']);
    disp(stats);
    disp(['mean observed',num2str(ValuesName),' =', num2str(mean(sortedObsVal))]);
    disp(['mean shuffled for DaysTolerance values',num2str(ValuesName)]);
    disp([DaysTolerance;        mean(squeeze(nanmean(SfValues,2)),1)]);
    disp([ 'correlation with the index: Rho is ',num2str(RHO),' pv=', num2str(PVAL)]);
    
    Diff=(nanmean(sortedShfldVal,2)-sortedObsVal)>0;
    Binom=min([binocdf(sum(Diff),length(Diff),0.5),1-binocdf(sum(Diff),length(Diff),0.5)]);%the minimum of larger or smaller
    disp('doing a binomial test how many individuals are bigger or smaller their mean shuffled value- any bias? ');
    disp(['n=',num2str(length(Diff)),' shfld>observed ',num2str(sum(Diff)),' two sided binom pv:',num2str(Binom)]);
    %[phat,pci]=binofit(sum(Diff), length(Diff),0.05);
    %corrplot([sortedObsVal,DiffValue])
        
    %% create figure 2 - observe/simulated Vs Shuffled over all season with boxplot
    figure2 = figure;
    axes2 = axes('Parent',figure2,'FontSize',10,'xtick',0:5:60);    % Create axes
    hold(axes2,'all');
    hggroup2 = hggroup('Parent',axes2);
    %plot(1:length(sortedObsVal),sortedObsVal,'linewidth',3,'color',[1 0 0]);
    title(['indiv ', ValuesName,' sorted by observed (red) and randomized (mean in blue) for time window ',...
        num2str(DaysTolerance(1)),' days, for data ',FieldName,' n=',num2str(length(obsValues)),...
        'Pv ttest=',num2str(p),' binom ',num2str(Binom)])
    
    %BOXPLOT
    %boxplot(sortedShfldVal');
    %hl=boxplot((sortedShfldVal'),'plotstyle','compact','symbol','');  %
    hl=boxplot((sortedShfldVal'),'boxstyle','outline','notch','off','symbol','');  %'plotstyle','compact'
    for ih=1:6;set(hl(ih,:),'LineWidth',3); end
    %a = get(get(gca,'children'),'children');   % Get the handles of all the objects
    t = get(hl,'tag');   % List the names of all the objects 
    Medianlines=hl(6:7:length(t));%getting locations of medians
    set(Medianlines, 'Color', 'k');   %changing their color to blue or black
    
    set(gca,'xtick',0:5:60,'XTickLabel',0:5:60);%
    plot(nanmean(sortedShfldVal,2),'linewidth',4,'Parent',hggroup2,'color',[0 0 1]) 
    plot(sortedObsVal,'linewidth',3,'color',[1 0 0],'Marker','o');
    %xlabel('sorted individuals')
    ylim([min([min(sortedObsVal),min(sortedShfldVal')]),max([max(sortedObsVal), max(sortedShfldVal')])]);
    xlim([1 length(sortedObsVal)+1]);
    %figure;hist(sortedObsVal-mean(sortedShfldVal,2),-3:0.5:3)  
    
     %ylabel(Name1,'FontSize',16);
     %set(findobj(gca,'Type','text'),'FontSize',14)
     
    %% Create figure 3 the network levelmean value. histog of the shufled data vs the observed. 
     Pnull=min([sum(nanmean(sortedShfldVal,1)<=mean(sortedObsVal)),sum(nanmean(sortedShfldVal,1)>=mean(sortedObsVal))])/shuffles
     figure3 = figure;
     axes3 = axes('Parent',figure3,'FontSize',12);    % Create axes 
     hold(axes3,'all');
     title(['Histogram. Pop mean ', ValuesName,' in shuffles',FieldName,' n=',num2str(size(sortedShfldVal,2)),'Pv null model=',num2str(Pnull)])
     hist(mean(sortedShfldVal,1),15)
     h = findobj(gca,'Type','patch');
     set(h,'FaceColor',[0 0 0.8],'EdgeColor',[0 0 0.8])
     %axis tight
     xlim([0.98* min([mean(obsValues) min(mean(sortedShfldVal,1))]), 1.02*max([mean(obsValues) max(mean(sortedShfldVal,1))])])
     xlabel(ValuesName);ylabel('Cases')
     line([mean(obsValues) mean(obsValues)],[0 max(hist(mean(sortedShfldVal,1),15))],'linestyle','--','linewidth',10,'Color',[1 0 0])
     set(gca,'view',[90 -90])
     
     %comparing the population level index using the distribution of the null model
%     fig=figure; 
%     hax=axes;hold on 
%     hist(nanmean(sortedShfldVal,1),20)
%     line([mean(sortedObsVal) mean(sortedObsVal)],get(hax,'YLim'),'Color',[1 0 0])
%     title(['A hist of populations ', ValuesName,' from shuffles (blue) and the obs value (red) ',...
%         'Pv null model=',num2str(Pnull)])
        
end

%% %%%%%SE+STD  looking on the effect of DaysTolerance on variation from the observed
if Type==3
    DiffInVals=SfValues-repmat(obsValues,[1,shuffles,length(DaysTolerance)]);
    DiffInVals2=squeeze(nanmean(DiffInVals,2));
    Yval=nanmean(DiffInVals2,1);
    Xval=DaysTolerance;
    
    %% Create figure 4 the effect of time window on the deviations between data and suffled data
    figure1 = figure;
    axes1 = axes('Parent',figure1,'FontSize',12,'xtick',0:20:120);    % Create axes
    hold(axes1,'all');
    xlim([0 1.02*max(Xval)])
    %SE
    title(['mean + SE of difference in ', ValuesName,' of observed (red) and randomized over different time scales',FieldName,' n=',num2str(length(obsValues))])
    %plot(DaysTolerance,nanmean(DiffInVals2,1),'LineStyle','none','marker','d','markersize',10,'MarkerFaceColor',[0,0,1])
    plot([min(Xval) max(Xval)],zeros(2,1),'--r')
    Err=nanstd(DiffInVals2)./sqrt(sum(~isnan(DiffInVals2)));%the std / sample size of non nan    
    errorbar(Xval,Yval,Err,'bo','markersize',10,'LineWidth',2,'MarkerFaceColor',[0,0,1])
    %errorbar(DaysTolerance,nanmean(DiffInVals2,1),Err,'LineStyle','none','LineWidth',2)
    
    %adding another ref line of 1/x for the relevant range if not 
    if (Yval(1)+Err(1))<0
       fitY=1./Xval;%this makes values of 1/x with the maximal value set as zero
       fitY=fitY*(max(Yval)-min(Yval))/(max(fitY)-min(fitY));%this step ensures the two sets have the same range
       fitY= fitY -(max(fitY)-max(Yval));%taking the fit down to the same range of Y
       plot(Xval,fitY,':k','LineWidth',3);           
    end
    
    %% Create figure 5, same as 4 but in log scale
    figure1 = figure;
    axes1 = axes('Parent',figure1,'FontSize',12,'xtick',[2,4,8,16, 32, 64, 128],'XScale','log','XMinorTick','on');    % Create axes
    hold(axes1,'all');
    xlim([1.8 1.03*max(Xval)])
    %SE
    title(['mean + SE of difference in ', ValuesName,' of observed (red) and randomized over different time scales',FieldName,' n=',num2str(length(obsValues))])
    %plot(DaysTolerance,nanmean(DiffInVals2,1),'LineStyle','none','marker','d','markersize',10,'MarkerFaceColor',[0,0,1])
    plot([min(Xval) max(Xval)],zeros(2,1),'--r')
    Err=nanstd(DiffInVals2)./sqrt(sum(~isnan(DiffInVals2)));%the std / sample size of non nan    
    errorbar(Xval,Yval,Err,'bo','markersize',10,'LineWidth',2,'MarkerFaceColor',[0,0,1])
    %errorbar(DaysTolerance,nanmean(DiffInVals2,1),Err,'LineStyle','none','LineWidth',2)
    
    %adding another ref line of 1/x for the relevant range if not 
    if (Yval(1)+Err(1))<0
       fitY=1./Xval;%this makes values of 1/x with the maximal value set as zero
       fitY=fitY*(max(Yval)-min(Yval))/(max(fitY)-min(fitY));%this step ensures the two sets have the same range
       fitY= fitY -(max(fitY)-max(Yval));%taking the fit down to the same range of Y
       plot(Xval,fitY,':k','LineWidth',3);           
    end

end

    
