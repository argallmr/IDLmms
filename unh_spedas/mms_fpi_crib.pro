;Pro load_mms_fpi
;Purpose: to load mms fpi data
;Inputs:
;file: cdf filename
Pro load_mms_fpi, file

cdf_leap_second_init
cdf2tplot,file

END

;Pro create_omni_spectra
;Purpose: To create omni spectra for e- or i+, average flux over six 
;directional fluxes. The newly
;generated tplot variable name is listed at the end
;Inputs:
;sat: 1-4, mms sc number
;specie: 'e' or 'i'
Pro create_omni_spectra, sat, specie

sat_str = string(sat,format='(I1)')
;use 'get_data' to get the data from the tplot variable
get_data,'mms'+sat_str+'_fpi_'+specie+'EnergySpectr_pX',data=dpx
get_data,'mms'+sat_str+'_fpi_'+specie+'EnergySpectr_mX',data=dmx
get_data,'mms'+sat_str+'_fpi_'+specie+'EnergySpectr_pY',data=dpy
get_data,'mms'+sat_str+'_fpi_'+specie+'EnergySpectr_mY',data=dmy
get_data,'mms'+sat_str+'_fpi_'+specie+'EnergySpectr_pZ',data=dpz
get_data,'mms'+sat_str+'_fpi_'+specie+'EnergySpectr_mZ',data=dmz

yomni = (dpx.y+dmx.y+dpy.y+dmy.y+dpz.y+dmz.y)/6.0
delv = (alog10(3.e4)-alog10(10.))/31
vs = alog10(10.)+findgen(32)*delv
v = 10.^vs

name = 'mms'+sat_str+'_fpi_'+specie+'EnergySpectr_omni'
;use 'store_data' to create a tplot variable
store_data,name,$
           data={x:dpx.x,y:yomni,v:v},$
           lim={spec:1, no_interp:1, $
                ytitle:'mms'+sat_str+'!C'+specie+' OMNI!C'+'(eV)',$
                ztitle:'Counts'}

ylim, name, 10,30000,1
zlim, name, 0,0,1
tplot_names,name
END

;Pro var_info
;Purpose: To get variable information
;Inputs:
;Required:
;file: cdf filename to load
;Optional:
;data (either input or output), already loaded data
;var: variable name or number in the data structure
;all_varname: if set, all variable names and their numbers in the
;struture will be printed
Pro var_info, file = file,$
              data = data,$
              var = var,$
              all_varname = all_varname

if ~keyword_set(data) then begin
     if ~keyword_set(file) then begin
        print,'The input cdf filename is required to load data'
        return
     endif
   data = cdf_load_vars(file,/all)
endif

if keyword_set(all_varname) then begin
   for i=0,data.nv-1 do print,i, data.vars[i].name
endif

if keyword_set(var) then begin
   if data_type(var) eq 7 then begin
      var = where(data.vars.name eq var)
   endif
   print,'********variable info:'
   help,data.vars[var]
   print,'*********variable data dimension:'
   help,*data.vars[var].dataptr
   print,'*********variable attributes:'
   help,*data.vars[var].attrptr
   print,'********var_notes:'
   print,(*data.vars[var].attrptr).var_notes
   print,'***********************************'
endif

END

Pro mms_fpi_crib

  ;Please specify the data_path and fname'
  data_path='/net/nfs/calypso/cluster10/swang/Data/cdf/'
  fname = 'mms3_fpi_fast_sitl_20150622000000_v0.0.0.cdf'
  file = data_path + fname

  sat = 1 ;mms sc number
  load_mms_fpi,file
  create_omni_spectra,sat,'e'
  create_omni_spectra,sat,'i'
  tplot,['mms1_fpi_eEnergySpectr_omni','mms1_fpi_iEnergySpectr_omni']

  ;Here are examples to print out variable attributes
  var = 'mms1_fpi_ePitchAngDist_lowEn'
  var_info,file=file,data=data,var=var,/all_varname

  var = 37
  var_info,data=data,var=var

END
