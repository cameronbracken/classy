function [n,rows,dem,minh,maxq,xl,xr]=getPars();
    % Return global parameters or set them if they are not set
    
    global N ROWS DEM MINH MAXQ XL XR BC Q_LIMITING
        
    if isempty(ROWS), rows = 4; else rows = ROWS; end
    if isempty(N), n = 10; else n = N; end
    if isempty(DEM), dem = 23.2; else dem = DEM; end
    if isempty(MINH), minh = .7; else minh = MINH; end
    if isempty(MAXQ)
        if(Q_LIMITING)
             maxq = 2.5;
        else
            maxq = 200;
        end
    else 
        maxq = MAXQ; 
    end
    
    if isempty(XL)
        switch BC
            case 1
                xl = linspace(1,1,rows);
            case 2
                xl = linspace(1-(rows-1)/10,1,rows); 
            case 3
                xl = 1-log(linspace(1-(rows-1)/10,1,rows));
        end
    else
        xl = XL;
    end
    
    if isempty(XR)
        switch BC
            case 1
                xr = linspace(1,1,rows);    
            case 2
                xr = linspace(1-(rows-1)/10,1,rows); 
            case 3
                xr = linspace(1-(rows-1)/10,1,rows);
        end
    else
        xr = XR;
    end
end