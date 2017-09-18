%load image
Dir_img = './Test results/IMG/';  
Imgs = dir(fullfile(Dir_img, '*.tif'));
%load GT
Dir_GT = './Test results/GT/';  
gts = dir(fullfile(Dir_GT, '*.tif .csv'));
%load data
Dir_data = './Test results/TT/';  
datas = dir(fullfile(Dir_data, '*.tif.csv'));
%Finaldata
Dir_final = './Test results/Training_data/'; 

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
                landmark(1,:) = [];
                %landmark way 2 ~~ full 16 
                landmark_way2 = zeros(1,2);
                landmark_way2(1,:) = [];
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
                     [m5 n5] = size(landmark_way2);
                    [value2 index2] = sort(temp4);
                    indices_2_10 = index2(2:10,:);
                    values_2_10 = value2(2:10,:);
                    landmark = [landmark;  D(index,:)  value transpose(values_2_10) transpose(indices_2_10)];

                    %start implement lanmark full 16 (fix)
                    [m5 n5] = size(landmark_way2);
                   
                    % add the first value to landmark
                    if (m5 < 1)
                        landmark_way2 = [landmark_way2;  D(index,:) value transpose(values_2_10) transpose(indices_2_10) ]  ;
                    else
                        %check new landmark isn't duplicate
                        if (all(D(index,1) ~= landmark_way2(:,1)) || all(D(index,2) ~= landmark_way2(:,2)))
                            landmark_way2 = [landmark_way2;  D(index,:) value transpose(values_2_10) transpose(indices_2_10)]; 
                        %if duplicate
                        else
                            [m6 n6]= size(landmark_way2);
                            for y= 1:m6
                                %find duplicate position
                                if ((D(index,1) == landmark_way2(y,1)) && (D(index,2) == landmark_way2(y,2)))
                                    if (value < landmark_way2(y,3))
                                       t_swap_data = landmark_way2(y,:);
                                       landmark_way2(y,:) = [D(index,:) value transpose(values_2_10) transpose(indices_2_10)];
                                       % if duplicated point is detected, check other distance to add a new point 
                                       loop_size = 9;
                                       for x = 1:loop_size
                                           % check_var = [  D(t_swap_data(:,x+12),:) t_swap_data(:,x+3)]
                                           if(all(D(t_swap_data(:,x+12),1) ~= landmark_way2(:,1)) || all(D(t_swap_data(:,x+12),2) ~= landmark_way2(:,2)))
                                               landmark_way2 = [landmark_way2;  D(t_swap_data(:,x+12),:)  t_swap_data(:,x+3) t_swap_data(:,4:end)];
                                               break
                                           end
                                       end
                                    else
                                       t_data = [D(index,:) value transpose(values_2_10) transpose(indices_2_10)];
                                       % if duplicated point is detected, check other distance to add a new point 
                                       loop_size = 9;
                                       for x = 1:loop_size
                                           %check_var = [  D(t_data(:,x+12),:) t_data(:,x+3)]
                                           if(all(D(t_data(:,x+12),1) ~= landmark_way2(:,1)) || all(D(t_data(:,x+12),2) ~= landmark_way2(:,2)))
                                               landmark_way2 = [landmark_way2;  D(t_data(:,x+12),:)  t_data(:,x+3) t_data(:,4:end)];
                                               break
                                           end
                                       end
                                    end
                                end
                            end
                        end
                    end
                    % end  landmark full 16 (fix)
                end
                landmark =landmark(:,1:2);
                landmark_way2= landmark_way2(:,1:2); 
                temp6 = zeros(length(D),1);
                data = [D temp6];

                for q = 1:m1
                    for p = 1:m2
                        if(landmark_way2(p,:) == D(q,:))   
                            data(q,3) = 1;
                        end
                    end
                end

                %{
                hold on;
                scatter(data(:,1),data(:,2));
                hold on;
                scatter(landmark(:,1), landmark(:,2));
                %}
                data = [data features im2single(feature2) features3 im2single(feature4)];
               
                %set name of feature to save 
                cHeader = {'X' 'Y' 'isLandmark'};
                cSURF= {};
                cBRISK ={};
                cFREAK ={};
                cHOG  ={};
                [num_row, num_col] = size(features);
                [num_row1, num_col1] = size(features3); 
                for i = 1:num_col
                   cSURF{end+1} = strcat('SURFfeature',num2str(i));
                   cBRISK{end+1} = strcat('BRISKfeature',num2str(i));
                   cFREAK{end+1} = strcat('FREAKfeature',num2str(i));
                end
                for i = 1:num_col1
                     cHOG{end+1} = strcat('HOGfeature',num2str(i));
                end
                cHeader= [cHeader cSURF  cBRISK cHOG cFREAK ];
                 
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
