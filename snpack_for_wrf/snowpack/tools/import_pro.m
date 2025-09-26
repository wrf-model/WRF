function param = import_pro(fn, sensor, p)
% function pro_param = import_pro(fn, sensor, p)
%
% fn = filename to .pro data.
%
% give sensor as a string! like '0522'
% read snwopack .pro-file with the given propertie field sensor
% if sensor = [], returns list of sensors as param.pro_param
%
% p.readings switches either to 
%    'all'   to read of full dataset
%    'last'   to read last entry
%
% see source code below what other fields in parameter 'p'can be changed.
%
% EXAMPLE:
% fn = '..\what_ever_profile_file.pro';
% sensor = '0505'; % 0506,nElems,liquid water content by volume (%)
% param = import_pro(fn, sensor, struct('readings', 'last'));
%
% TODO: 
%  .include efficient search to read only one time entry
%   (e.g. use grep http://www.mathworks.com/matlabcentral/fileexchange/9647-grep--a-pedestrian--very-fast-grep-utility/content//grep.m
%
%
% 05.04.2015
% initial write set
%
% 29.05.2015
% - bugfix with sensors return
% - include readings
% Anselm

if nargin < 2
    sensor = []; % return the list of sensors codes
end
if nargin < 3
  p = [];
end

dp.readings = 'all';
dp.maxblocklength = 1000;  % just the number of byte to go back in file to get only the last time step... quick and dirty...
                           % if profile block length is larger, it increases the blocklength by factor 2 until is big enough.
dp.timefield = '0500';
p = combine_struct_intern(dp,p);

% read header and find number header lines
fnid = fopen(fn, 'r');
headerline = 'start';
lnr = 0;
pro_param = {};

while ischar(headerline)
    headerline = fgetl(fnid); lnr = lnr + 1 ; % headerlines
    %headerfield = regexp(headerline, '=.');
    
    if strcmpi(headerline, '[STATION_PARAMETERS]')
         while ~isempty(headerline)
             headerline = fgetl(fnid); lnr = lnr + 1 ; % headerlines
             field_id = regexp(headerline, '= ');
             if ~isempty(field_id)
                 field_value = headerline(field_id+2:end);
                 if ~isnan(str2double(field_value))
                    field_value = str2double(headerline(field_id+2:end));
                 end
                 param.(headerline(1:field_id-1)) = field_value;
             end
         end
    end
    
    
    if strcmpi(headerline,'[DATA]') % skip when data block comes
        break;
    end
    
    % return list of field sensors if no sensor is given
    if isempty(sensor)
                    
        if strcmpi(headerline, '[HEADER]')
            headerline = fgetl(fnid); lnr = lnr + 1 ; % headerlines
            
            while ~isempty(headerline)

                headerline = fgetl(fnid); lnr = lnr + 1 ; % headerlines
                pro_param = [pro_param; headerline]; 
                
            end
            warning('no sensor field defined! returning possible sensor fields!')
            lay_param = [];
            break;
            
        end    
    end 
end
%fclose(fnid);

if ~isempty(pro_param)
    fclose(fnid);
    param.pro_param = pro_param;
    return;
end

% jump in file to faster read only last point!
if strcmpi(p.readings, 'last')
  lastpost = ftell(fnid);  
  while 1
    status = fseek(fnid, -p.maxblocklength, 'eof'); % go to end - maxblocklength
    if status
      status = fseek(fnid, lastpoint, 'bof'); % go to end of header if short file
    end
    % check if p.maxblocklength was long enough
    ddata = fread(fnid,inf,'*char')';
    if ~isempty(regexp(ddata, ['\n' p.timefield], 'once') )
        status = fseek(fnid, -p.maxblocklength, 'eof'); % go to end - maxblocklength
      if status
        status = fseek(fnid, lastpoint, 'bof'); % go to end of header if short file
      end
      break;
    end
    p.maxblocklength = 2*p.maxblocklength;
  end
end

% read datablock line by line!
pro_param = []; lay_param = []; n = 0;
dataline = 'start data';
while ischar(dataline)
  % read lines
      dataline = fgetl(fnid); lnr = lnr + 1 ;

  % break condition
    if dataline == -1
        break
    end
    
    sensorfield = textscan(dataline, '%4s,%*s', 'Delimiter', ',');
    
    % catch date and time
    if strcmp(p.timefield, sensorfield{1})
      try  
        date_time = datenum(dataline(6:end), 'dd.mm.yyyy HH:MM:SS');
      catch
        date_time = datenum(dataline(6:end), 'dd.mm.yyyy HH:MM');
      end
    end
    
    % catch the layerthickness
    if strcmp('0501', sensorfield{1})
        lay_values = textscan(dataline(6:end), '%f', 'Delimiter', ',');
        lay_values =  lay_values{1}(2:end)';
    end
    
    % first catch the date_time, then the sensor to properly align them!
    if strcmp(sensor, sensorfield{1})
        sensor_values = textscan(dataline(6:end), '%f', 'Delimiter', ',');
        sensor_values =  sensor_values{1}(2:end)';
        pro_param(size(pro_param, 1)+1, 1:length(sensor_values)+1) = [date_time, sensor_values];
        % only write layers when the requested sensorfield is found...
        lay_param(size(lay_param, 1)+1, 1:length(lay_values)+1) = [date_time, lay_values];
    end    
end
fclose(fnid);

if strcmpi(p.readings, 'last')
  pro_param = pro_param(end, :);
  lay_param = lay_param(end, :);
end

param.pro_param = pro_param;
param.lay_thick = lay_param;

return;

function a=combine_struct_intern(a,varargin)
%COMBINE_STRUCT(b,vargin) returns b and then overwrites with
% following structures
  for n=1:length(varargin)
    c = varargin{n};
    for j=1:length(c)
      nms = fieldnames(c(j));
      for m=1:length(nms)
        if isfield(a,nms{m})
          if isstruct(a(j).(nms{m})) & isstruct(c(j).(nms{m}))
            a(j).(nms{m})=combine_struct(a(j).(nms{m}),c(j).(nms{m})); 
          else
            a(j).(nms{m})=c(j).(nms{m});
          end
        else
          a(j).(nms{m})=c(j).(nms{m});
        end	
      end
    end
  end
end



fn = '~/Dropbox/data/meteo_smet/meteo/anselm/visualize.SNP/input/3D/ori_816/68_54_VDLS.pro';
param = import_pro(fn, '0505', struct('readings', 'last'));

end