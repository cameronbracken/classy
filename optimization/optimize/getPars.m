function [n,rows,dem,minh,maxq,xl,xr]=getPars
    % Return global parameters or set them if they are not set
    
    global N ROWS DEM MINH MAXQ XL XR setting
        
    if isempty(ROWS), rows = 4; else rows = ROWS; end
    if isempty(N), n = 10; else n = N; end
    if isempty(DEM), dem = 5; else dem = DEM; end
    if isempty(MINH), minh = .7; else minh = MINH; end
    if isempty(MAXQ), maxq = .25; else maxq = MAXQ; end
    
    if isempty(XL)
        if setting == 1
            xl = linspace(1,1,rows);
        elseif setting == 2
            xl = linspace(1-(rows-1)/10,1,rows); 
        elseif setting == 3
            xl = 1-log(linspace(1-(rows-1)/10,1,rows));
        end
    else
        xl = XL;
    end
    
    if isempty(XR)
        if setting == 1
            xr = linspace(1,1,rows);
        elseif setting == 2
            xr = linspace(1-(rows-1)/10,1,rows); 
        elseif setting == 3
            xr = linspace(1-(rows-1)/10,1,rows);
        end
    else
        xr = XR;
    end
end