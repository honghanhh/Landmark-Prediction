%load image
Dir_img = './Dataset/';  
Imgs = dir(fullfile(Dir_img, '*.tif'));
%load GT
Dir_GT = './Groundtruth/';  
gts = dir(fullfile(Dir_GT, '*.tif .csv'));
%load data
Dir_data = './Data/';  
datas = dir(fullfile(Dir_data, '*.tif.csv'));
%Finaldata
Dir_final = './TrainingData/'; 

for i=1:length(Imgs)
    img_name = strsplit(Imgs(i).name, '.tif');
    img_name = img_name(1);
    for j=1:length(gts)
        gt_name = strsplit(gts(j).name,  '.tif');
        gt_name = gt_name(1);
        for k = 1:length(datas)
            data_name = strsplit(datas(k).name, '.tif');
            data_name = data_name(1);
            if ((strcmp(img_name, gt_name) == 1) && (strcmp(img_name,data_name)==1))
                %read img
                I= imread(fullfile(Dir_img, Imgs(i).name));
                I_gray=rgb2gray(I);
                %read GT
                G = readtable(fullfile(Dir_GT, gts(j).name));
                groundtruth = [G.X G.Y];
                %read data
                D = readtable(fullfile(Dir_data, datas(k).name));
                D = table2array(D);
                %computation...
                temp7 = zeros(length(groundtruth),1);
                temp7(:,1) = length(I(:,1));
                groundtruth = [groundtruth(:,1), temp7 - groundtruth(:,2)];
                %subplot(1,length(Imgs),i);
                %imshow(I);
                %hold on; 
                %scatter(groundtruth(:,1),groundtruth(:,2));
                %Convert to SURF POINT type and extract features
                points = SURFPoints(D);
                [features, valid_points] = extractFeatures(I_gray, points);
                
                points1 = SURFPoints(groundtruth);
                [features1, valid_points1] = extractFeatures(I_gray, points1);
                
                %BRISK POINT - binary descriptors
                points2 = BRISKPoints(D);
                [features2, valid_points2] = extractFeatures(I_gray, points2);
                feature2 = features2.Features; 
                
                %HOG - using SURFPoints ( SURF/BRISK/D - same results?)
                [features3,validPoints3] = extractHOGFeatures(I_gray,points);
                          
                %FREAK - BRISKPoints (BRISKPoints/D - same results) - binary descriptors
                features4 = extractFeatures(I_gray, points2, 'Method', 'FREAK');
                feature4 = features4.Features;
                %MSER
                %regions = MSERRegions(points);
                %[features5, valid_points5] = extractFeatures(I_gray,regions,'Upright',true);
                %SIFT
                %PCA-SIFT 
                %LBP
                %extractLBPFeatures
                %GLOH
                %DAISY
                %Shape Contexts
                %Color Histograms
                
                
                %Calculate distance in 64 dimensions
                E = features;
                F= features1;
                [m1,n1]= size(E);
                [m2,n2] = size(F);

                %initial variable
                temp1 = zeros(length(features),1);
                temp2 = zeros(length(features1),1);
                LMdistance = zeros(length(E),1);
                LMdistance(:,1) = [];
                %starting calculation
                for o = 1:m2
                    %calculate distance for 1 features of landmark to all features
                    for p = 1:m1
                       temp = 0;
                       for q = 1:n1
                        temp = temp +(E(p,q)-F(o,q))^2;
                       end
                       distance = sqrt(temp);
                       temp1(p,1) =  distance;
                    end
                    LMdistance = [LMdistance temp1];
                end

                %Calculate distance in 2 dimensions
                [m3,n3]=size(D);
                [m4,n4] = size(groundtruth);
                %initial variable
                temp4 = zeros(length(D),1);
                temp5 = zeros(length(groundtruth),1);
                landmark = zeros(1,2);
                %starting calculation
                format long g
                for o = 1:m4
                    %calculate distance for 1 features of landmark to all features
                    for p = 1:m3
                       temp3 = 0;
                       for q = 1:n3
                        temp3 = temp3 +(D(p,q)-groundtruth(o,q))^2;
                       end
                       distance = sqrt(temp3);
                       temp4(p,1) =  distance;
                    end
                    [value index] = min(temp4);
                    landmark = [landmark;  D(index,:)];
                end


                landmark(1,:) = [];
                temp6 = zeros(length(D),1);
                data = [D temp6];

                for q = 1:m1
                    for p = 1:m2
                        if(landmark(p,:) == D(q,:))   
                            data(q,3) = 1;
                        end
                    end
                end

                
                hold on;
                scatter(data(:,1),data(:,2));
                hold on;
                scatter(landmark(:,1), landmark(:,2));
                
                data = [data features im2single(feature2) features3 im2single(feature4)];

                cHeader = {'X' 'Y' 'isLandmark','SURFfeature1','SURFfeature2','SURFfeature3','SURFfeature4','SURFfeature5',...
                           'SURFfeature6','SURFfeature7','SURFfeature8','SURFfeature9','SURFfeature10','SURFfeature11',...
                           'SURFfeature12','SURFfeature13','SURFfeature14','SURFfeature15','SURFfeature16','SURFfeature17',...
                           'SURFfeature18','SURFfeature19','SURFfeature20','SURFfeature21','SURFfeature22','SURFfeature23',...
                           'SURFfeature24','SURFfeature25','SURFfeature26','SURFfeature27','SURFfeature28','SURFfeature29',...
                           'SURFfeature30','SURFfeature31','SURFfeature32','SURFfeature33','SURFfeature34','SURFfeature35',...
                           'SURFfeature36','SURFfeature37','SURFfeature38','SURFfeature39','SURFfeature40','SURFfeature41',...
                           'SURFfeature42','SURFfeature43','SURFfeature44','SURFfeature45','SURFfeature46','SURFfeature47',...
                           'SURFfeature48','SURFfeature49','SURFfeature50','SURFfeature51','SURFfeature52','SURFfeature53',...
                           'SURFfeature54','SURFfeature55','SURFfeature56','SURFfeature57','SURFfeature58','SURFfeature59',...
                           'SURFfeature60','SURFfeature61','SURFfeature62','SURFfeature63','SURFfeature64','BRISKfeature1',...
                           'BRISKfeature2','BRISKfeature3','BRISKfeature4','BRISKfeature5','BRISKfeature6','BRISKfeature7',...
                           'BRISKfeature8','BRISKfeature9','BRISKfeature10','BRISKfeature11','BRISKfeature12',...
                           'BRISKfeature13','BRISKfeature14','BRISKfeature15','BRISKfeature16','BRISKfeature17',...
                           'BRISKfeature18','BRISKfeature19','BRISKfeature20','BRISKfeature21','BRISKfeature22',...
                           'BRISKfeature23','BRISKfeature24','BRISKfeature25','BRISKfeature26','BRISKfeature27',...
                           'BRISKfeature28','BRISKfeature29','BRISKfeature30','BRISKfeature31','BRISKfeature32',...
                           'BRISKfeature33','BRISKfeature34','BRISKfeature35','BRISKfeature36','BRISKfeature37',...
                           'BRISKfeature38','BRISKfeature39','BRISKfeature40','BRISKfeature41','BRISKfeature42',...
                           'BRISKfeature43','BRISKfeature44','BRISKfeature45','BRISKfeature46','BRISKfeature47',...
                           'BRISKfeature48','BRISKfeature49','BRISKfeature50','BRISKfeature51','BRISKfeature52',...
                           'BRISKfeature53','BRISKfeature54','BRISKfeature55','BRISKfeature56','BRISKfeature57',...
                           'BRISKfeature58','BRISKfeature59','BRISKfeature60','BRISKfeature61','BRISKfeature62',...
                           'BRISKfeature63','BRISKfeature64','HOGfeature1','HOGfeature2','HOGfeature3','HOGfeature4',...
                           'HOGfeature5','HOGfeature6','HOGfeature7','HOGfeature8','HOGfeature9','HOGfeature10','HOGfeature11',...
                           'HOGfeature12','HOGfeature13','HOGfeature14','HOGfeature15','HOGfeature16','HOGfeature17','HOGfeature18',...
                           'HOGfeature19','HOGfeature20','HOGfeature21','HOGfeature22','HOGfeature23','HOGfeature24','HOGfeature25',...
                           'HOGfeature26','HOGfeature27','HOGfeature28','HOGfeature29','HOGfeature30','HOGfeature31','HOGfeature32',...
                           'HOGfeature33','HOGfeature34','HOGfeature35','HOGfeature36','FREAKfeature1',...
                           'FREAKfeature2','FREAKfeature3','FREAKfeature4','FREAKfeature5','FREAKfeature6','FREAKfeature7',...
                           'FREAKfeature8','FREAKfeature9','FREAKfeature10','FREAKfeature11','FREAKfeature12',...
                           'FREAKfeature13','FREAKfeature14','FREAKfeature15','FREAKfeature16','FREAKfeature17',...
                           'FREAKfeature18','FREAKfeature19','FREAKfeature20','FREAKfeature21','FREAKfeature22',...
                           'FREAKfeature23','FREAKfeature24','FREAKfeature25','FREAKfeature26','FREAKfeature27',...
                           'FREAKfeature28','FREAKfeature29','FREAKfeature30','FREAKfeature31','FREAKfeature32',...
                           'FREAKfeature33','FREAKfeature34','FREAKfeature35','FREAKfeature36','FREAKfeature37',...
                           'FREAKfeature38','FREAKfeature39','FREAKfeature40','FREAKfeature41','FREAKfeature42',...
                           'FREAKfeature43','FREAKfeature44','FREAKfeature45','FREAKfeature46','FREAKfeature47',...
                           'FREAKfeature48','FREAKfeature49','FREAKfeature50','FREAKfeature51','FREAKfeature52',...
                           'FREAKfeature53','FREAKfeature54','FREAKfeature55','FREAKfeature56','FREAKfeature57',...
                           'FREAKfeature58','FREAKfeature59','FREAKfeature60','FREAKfeature61','FREAKfeature62',...
                           'FREAKfeature63','FREAKfeature64'};
                
                commaHeader = [cHeader;repmat({','},1,numel(cHeader))];
                commaHeader = commaHeader(:)';
                textHeader = cell2mat(commaHeader);
        
                %write header to file
                fid = fopen(fullfile(Dir_final, datas(k).name),'w'); 
                fprintf(fid,'%s\n',textHeader);
                fclose(fid);
                
                %write data to end of file
                dlmwrite(fullfile(Dir_final, datas(k).name),data,'-append'); 

            end
        end
    end

end
