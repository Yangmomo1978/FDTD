  
  set(0, 'defaultfigurevisible','off');
  
%  fp = fopen('output/Limits.txt','r');
%  limits = fscanf(fp,'%f');  
  limits = [-30, 30];
  i = 1;
  for t = 100:200   
    %Make ColorMap
    num = num2str(t);
    file = strcat('output/',num,'.csv');
    M = csvread(file);
    M = M(:,1:end-1);
    h = surf(M,'EdgeColor','None','facecolor','interp'); 
    colormap(jet);
    caxis([limits(1),limits(2)]);
    colorbar;
    view(2);
 
    mov(i+1) = getframe(gcf);
    i = i + 1;
  end
  
  movie2avi(mov,'WaveMovie.avi');

exit;
