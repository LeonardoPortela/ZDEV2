*&--------------------------------------------------------------------&*
*&                         Consultoria                                &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: CAMILA BRAND                                            &*
*& Data.....: 24/03/2022                                              &*
*& Descrição: COCKPIT – CARGA PM LOCAL DE INSTALAÇÃO & EQUIPAMENTO    &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT zpmr0077 MESSAGE-ID zcarga.

"Tabelas
TABLES: zpmt0063, zpmt0030,
        t370s,
        t370f, sscrfields.

DATA: gwa_validdate       TYPE bapi_itob_parms-inst_date,
      gwa_dataspecific    LIKE bapi_itob_eq_only,
      gwa_data_specificx  TYPE bapi_itob_eq_onlyx,
      gwa_datageneral     LIKE bapi_itob,
      gwa_data_generalx   TYPE bapi_itobx,
      gwa_datafleet       LIKE bapi_fleet,
      gwa_data_fleetx     LIKE bapi_fleetx,
      gwa_datageneralexp  LIKE bapi_itob,
      gwa_dataspecificexp LIKE bapi_itob_eq_only,
      gwa_datainstall     LIKE bapi_itob_eq_install,
      gwa_return          LIKE bapiret2,
      gwa_datafleetexp    LIKE bapi_fleet,
      gwa_externalnumber  TYPE bapi_itob_parms-equipment,
      git_xtensionin      TYPE TABLE OF bapiparex.

TYPES: BEGIN OF ty_saida_local_inst_001,
         grupo      TYPE char4,
         empresa    TYPE char4,
         centro     TYPE char4,
         setor      TYPE char4,
         eqsup      TYPE char5,
         eqinf      TYPE char5,
         funcloc    TYPE bapi_itob_parms-funcloc,
         strind     TYPE bapi_itob_fl_only-strind,
         category   TYPE bapi_itob_fl_only-category,
         pltxt      TYPE pltxt,
         authgrp    TYPE bapi_itob-authgrp,
         obj_weight TYPE bapi_itob-obj_weight,
         unit_of_wt TYPE bapi_itob-unit_of_wt,
         obj_size   TYPE bapi_itob-obj_size,
         inventory  TYPE bapi_itob-inventory,
         acqdate    TYPE bapi_itob-acqdate,
         objecttype TYPE bapi_itob-objecttype,
         acquisval  TYPE bapi_itob-acquisval,
         currency   TYPE bapi_itob-currency,
         ansdt      TYPE bapi_itob-acqdate,
         manfacture TYPE bapi_itob-manfacture,
         mancountry TYPE bapi_itob-mancountry,
         manmodel   TYPE bapi_itob-manmodel,
         constyear  TYPE bapi_itob-constyear,
         constmonth TYPE bapi_itob-constmonth,
         manparno   TYPE bapi_itob-manparno,
         manserno   TYPE bapi_itob-manserno,
         maintplant TYPE bapi_itob-maintplant,
         maintloc   TYPE bapi_itob-maintloc,
         maintroom  TYPE bapi_itob-maintroom,
         plsectn    TYPE bapi_itob-plsectn,
         work_ctr   TYPE c LENGTH 8,
         abcindic   TYPE bapi_itob-abcindic,
         sortfield  TYPE bapi_itob-sortfield,
         comp_code  TYPE bapi_itob-comp_code,
         bus_area   TYPE bapi_itob-bus_area,
         asset_no   TYPE bapi_itob-asset_no,
         sub_number TYPE bapi_itob-sub_number,
         costcenter TYPE bapi_itob-costcenter,
         wbs_elem   TYPE c LENGTH 8,
         standorder TYPE bapi_itob-standorder,
         settlorder TYPE bapi_itob-settlorder,
         planplant  TYPE bapi_itob-planplant,
         plangroup  TYPE bapi_itob-plangroup,
         gewrk      TYPE gewrk,
         wergw      TYPE wergw,
         catprofile TYPE bapi_itob-catprofile,
         eqinstall  TYPE bapi_itob_fl_only-eqinstall,
         supfloc    TYPE bapi_itob_fl_only-supfloc,
         consttype  TYPE bapi_itob-consttype,
         posnr      TYPE bapi_itob_fl_only-posnr,
         eqsingle   TYPE bapi_itob_fl_only-eqsingle,
       END OF ty_saida_local_inst_001.

TYPES: BEGIN OF ty_saida_local_inst_002,
         grupo      TYPE char4,
         empresa    TYPE char4,
         centro     TYPE char4,
         setor      TYPE char4,
         eqsup      TYPE char5,
         eqinf      TYPE char5,
         funcloc    TYPE bapi_itob_parms-funcloc,
         strind     TYPE bapi_itob_fl_only-strind,
         category   TYPE bapi_itob_fl_only-category,
         pltxt      TYPE pltxt,
         authgrp    TYPE bapi_itob-authgrp,
         obj_weight TYPE bapi_itob-obj_weight,
         unit_of_wt TYPE bapi_itob-unit_of_wt,
         obj_size   TYPE bapi_itob-obj_size,
         inventory  TYPE bapi_itob-inventory,
         acqdate    TYPE bapi_itob-acqdate,
         objecttype TYPE bapi_itob-objecttype,
         acquisval  TYPE bapi_itob-acquisval,
         currency   TYPE bapi_itob-currency,
         ansdt      TYPE bapi_itob-acqdate,
         manfacture TYPE bapi_itob-manfacture,
         mancountry TYPE bapi_itob-mancountry,
         manmodel   TYPE bapi_itob-manmodel,
         constyear  TYPE bapi_itob-constyear,
         constmonth TYPE bapi_itob-constmonth,
         manparno   TYPE bapi_itob-manparno,
         manserno   TYPE bapi_itob-manserno,
         maintplant TYPE bapi_itob-maintplant,
         maintloc   TYPE bapi_itob-maintloc,
         maintroom  TYPE bapi_itob-maintroom,
         plsectn    TYPE bapi_itob-plsectn,
         work_ctr   TYPE c LENGTH 8,
         abcindic   TYPE bapi_itob-abcindic,
         sortfield  TYPE bapi_itob-sortfield,
         comp_code  TYPE bapi_itob-comp_code,
         bus_area   TYPE bapi_itob-bus_area,
         asset_no   TYPE bapi_itob-asset_no,
         sub_number TYPE bapi_itob-sub_number,
         costcenter TYPE bapi_itob-costcenter,
         wbs_elem   TYPE c LENGTH 8,
         standorder TYPE bapi_itob-standorder,
         settlorder TYPE bapi_itob-settlorder,
         planplant  TYPE bapi_itob-planplant,
         plangroup  TYPE bapi_itob-plangroup,
         gewrk      TYPE gewrk,
         wergw      TYPE wergw,
         catprofile TYPE bapi_itob-catprofile,
         eqinstall  TYPE bapi_itob_fl_only-eqinstall,
         supfloc    TYPE bapi_itob_fl_only-supfloc,
         consttype  TYPE bapi_itob-consttype,
         posnr      TYPE bapi_itob_fl_only-posnr,
         eqsingle   TYPE bapi_itob_fl_only-eqsingle,
       END OF ty_saida_local_inst_002.

TYPES: BEGIN OF ty_locations,
         tplnr TYPE iflo-tplnr,
         pltxt TYPE iflo-pltxt,
       END OF ty_locations.

DATA locations TYPE TABLE OF ty_locations.
************************************************ EQUIPAMENTOS*************************************************
* Equipamentos Indústria, TI, Oficinas, Pneus, Agregados
TYPES: BEGIN OF ty_saida_cat01,
         id              TYPE p,
         status          TYPE char05,
         desc_status     TYPE char50,
         external_number TYPE bapi_itob_parms-equipment,
         valid_date      TYPE bapi_itob_parms-inst_date,
         equicatgry      TYPE bapi_itob_eq_only-equicatgry,
         authgrp         TYPE bapi_itob-authgrp,
         obj_weight      TYPE bapi_itob-obj_weight,
         unit_of_wt      TYPE bapi_itob-unit_of_wt,
         obj_size        TYPE bapi_itob-obj_size,
         inventory       TYPE bapi_itob-inventory,
         start_from      TYPE bapi_itob-start_from,
         objecttype      TYPE bapi_itob-objecttype,
         acquisval       TYPE bapi_itob-acquisval,
         currency        TYPE bapi_itob-currency,
         acqdate         TYPE bapi_itob-acqdate,
         manfacture      TYPE bapi_itob-manfacture,
         mancountry      TYPE bapi_itob-mancountry,
         manmodel        TYPE bapi_itob-manmodel,
         constyear       TYPE bapi_itob-constyear,
         constmonth      TYPE bapi_itob-constmonth,
         manparno        TYPE bapi_itob-manparno,
         manserno        TYPE bapi_itob-manserno,
         descript        TYPE bapi_itob-descript,
         datab           TYPE bapi_itob-start_from, "Verificar ABAP
         maintplant      TYPE bapi_itob-maintplant,
         maintloc        TYPE bapi_itob-maintloc,
         maintroom       TYPE bapi_itob-maintroom, "Verificar ABAP
         plsectn         TYPE bapi_itob-plsectn,
         pp_wkctr        TYPE bapi_itob-pp_wkctr,  "Verificar ABAP
         abcindic        TYPE bapi_itob-abcindic,
         sortfield       TYPE bapi_itob-sortfield,
         comp_code       TYPE bapi_itob-comp_code,
         bus_area        TYPE bapi_itob-bus_area,
         asset_no        TYPE bapi_itob-asset_no,
         planplant       TYPE bapi_itob-planplant,
         sub_number      TYPE bapi_itob-sub_number,
         costcenter      TYPE bapi_itob-costcenter,
         wbs_elem        TYPE bapi_itob-wbs_elem,
         standorder      TYPE bapi_itob-standorder,
         settlorder      TYPE bapi_itob-settlorder,
         plangroup       TYPE bapi_itob-plangroup,
         work_ctr        TYPE bapi_itob-work_ctr,
         catprofile      TYPE bapi_itob-catprofile,
         wergw           TYPE bapi_itob-pp_wkctr,        "Verificar abap
         datum           TYPE bapi_itob_parms-inst_date, "Verificar abap
         uzeit           TYPE bapi_itob_parms-inst_time, "Verificar abap
         read_floc       TYPE bapi_itob_eq_only-read_floc,
         consttype       TYPE bapi_itob-consttype,
         inst_pos        TYPE bapi_itob_eq_only-inst_pos,
         techid          TYPE bapi_itob_eq_only-techid,
         flag            TYPE c,
       END OF ty_saida_cat01.

TYPES: BEGIN OF ty_saida_cat02,
         id               TYPE p,
         status           TYPE char05,
         external_number  TYPE bapi_itob_parms-equipment,
         valid_date       TYPE bapi_itob_parms-inst_date,
         equicatgry       TYPE bapi_itob_eq_only-equicatgry,
         authgrp          TYPE bapi_itob-authgrp,
         obj_weight       TYPE bapi_itob-obj_weight,
         unit_of_wt       TYPE bapi_itob-unit_of_wt,
         obj_size         TYPE bapi_itob-obj_size,
         inventory        TYPE bapi_itob-inventory,
         start_from       TYPE bapi_itob-start_from,
         objecttype       TYPE bapi_itob-objecttype,
         acquisval        TYPE bapi_itob-acquisval,
         currency         TYPE bapi_itob-currency,
         acqdate          TYPE bapi_itob-acqdate,
         manfacture       TYPE bapi_itob-manfacture,
         mancountry       TYPE bapi_itob-mancountry,
         manmodel         TYPE bapi_itob-manmodel,
         constyear        TYPE bapi_itob-constyear,
         constmonth       TYPE bapi_itob-constmonth,
         manparno         TYPE bapi_itob-manparno,
         manserno         TYPE bapi_itob-manserno,
         descript         TYPE bapi_itob-descript,
         datab            TYPE bapi_itob-start_from, "Verificar ABAP
         maintplant       TYPE bapi_itob-maintplant,
         maintloc         TYPE bapi_itob-maintloc,
         maintroom        TYPE bapi_itob-maintroom,  "Verificar ABAP
         plsectn          TYPE bapi_itob-plsectn,
         pp_wkctr         TYPE bapi_itob-pp_wkctr,  "Verificar ABAP
         abcindic         TYPE bapi_itob-abcindic,
         sortfield        TYPE bapi_itob-sortfield,
         comp_code        TYPE bapi_itob-comp_code,
         bus_area         TYPE bapi_itob-bus_area,
         asset_no         TYPE bapi_itob-asset_no,
         costcenter       TYPE bapi_itob-costcenter,
         wbs_elem         TYPE bapi_itob-wbs_elem,
         standorder       TYPE bapi_itob-standorder,
         settlorder       TYPE bapi_itob-settlorder,
         planplant        TYPE bapi_itob-planplant,
         plangroup        TYPE bapi_itob-plangroup,
         work_ctr         TYPE bapi_itob-work_ctr,
         catprofile       TYPE bapi_itob-catprofile,
         read_floc        TYPE bapi_itob_eq_only-read_floc,
         datum            TYPE bapi_itob_parms-inst_date, "Verificar abap
         uzeit            TYPE bapi_itob_parms-inst_time, "Verificar abap
         consttype        TYPE bapi_itob-consttype,
         inst_pos         TYPE bapi_itob_eq_only-inst_pos,
         techid           TYPE bapi_itob_eq_only-techid,
         fleet_num        TYPE bapi_fleet-fleet_num,
         license_num      TYPE bapi_fleet-license_num,
         expiry_date      TYPE bapi_fleet-expiry_date,
         fleet_vin        TYPE bapi_fleet-fleet_vin,
         chassis_num      TYPE bapi_fleet-chassis_num,
         gross_wgt        TYPE bapi_fleet-gross_wgt,
         load_wgt         TYPE bapi_fleet-load_wgt,
         load_vol         TYPE bapi_fleet-load_vol,
         vol_unit         TYPE bapi_fleet-vol_unit,
         load_hgt         TYPE bapi_fleet-load_hgt,
         load_dim_unit    TYPE bapi_fleet-load_dim_unit,
         load_wid         TYPE bapi_fleet-load_wid,
         load_len         TYPE bapi_fleet-load_len,
         no_compart       TYPE bapi_fleet-no_compart,
         fleet_hgt        TYPE bapi_fleet-fleet_hgt,
         dim_unit         TYPE bapi_fleet-dim_unit,
         fleet_wid        TYPE bapi_fleet-fleet_wid,
         fleet_len        TYPE bapi_fleet-fleet_len,
         repla_date       TYPE bapi_fleet-repla_date,
         repla_odom       TYPE bapi_fleet-repla_odom,
         repla_oph        TYPE bapi_fleet-repla_oph,
         fleet_use        TYPE bapi_fleet-fleet_use,
         card_num         TYPE bapi_fleet-card_num,
         max_occupants    TYPE bapi_fleet-max_occupants,
         key_num          TYPE bapi_fleet-key_num,
         num_axle         TYPE bapi_fleet-num_axle,
         engine_type      TYPE bapi_fleet-engine_type,
         engine_snr       TYPE bapi_fleet-engine_snr,
         speed_max        TYPE bapi_fleet-speed_max,
         speed_unit       TYPE bapi_fleet-speed_unit,
         engine_power     TYPE bapi_fleet-engine_power,
         unit_power       TYPE bapi_fleet-unit_power,
         revolutions      TYPE bapi_fleet-revolutions,
         engine_cap       TYPE bapi_fleet-engine_cap,
         unit_cap         TYPE bapi_fleet-unit_cap,
         engine_cyl       TYPE bapi_fleet-engine_cyl,
         fuel_pri         TYPE bapi_fleet-fuel_pri,
         fuel_sec         TYPE bapi_fleet-fuel_sec,
         oil_type         TYPE bapi_fleet-oil_type,
         pri_calc         TYPE bapi_fleet-pri_calc,
         div1             TYPE fleet-div1, "(Append tabela Fleet)
         tq_combustivel_1 TYPE fleet-tq_combustivel_1,  "(Append tabela Fleet)
         div2             TYPE fleet-div2,  "(Append tabela Fleet)
         tq_combustivel_2 TYPE fleet-tq_combustivel_2,  "(Append tabela Fleet)
         div3             TYPE fleet-div3, "(Append tabela Fleet)
         tq_combustivel_3 TYPE fleet-tq_combustivel_3,  "(Append tabela Fleet)
         flag             TYPE c,
       END OF ty_saida_cat02.

DATA:
  BEGIN OF registros_cat01 OCCURS 1,
    valid_date TYPE bapi_itob_parms-inst_date,
    equicatgry TYPE bapi_itob_eq_only-equicatgry,
    authgrp    TYPE bapi_itob-authgrp,
    obj_weight TYPE bapi_itob-obj_weight,
    unit_of_wt TYPE bapi_itob-unit_of_wt,
    obj_size   TYPE bapi_itob-obj_size,
    inventory  TYPE bapi_itob-inventory,
    start_from TYPE bapi_itob-start_from,
    objecttype TYPE bapi_itob-objecttype,
    acquisval  TYPE bapi_itob-acquisval,
    currency   TYPE bapi_itob-currency,
    acqdate    TYPE bapi_itob-acqdate,
    manfacture TYPE bapi_itob-manfacture,
    mancountry TYPE bapi_itob-mancountry,
    manmodel   TYPE bapi_itob-manmodel,
    constyear  TYPE bapi_itob-constyear,
    constmonth TYPE bapi_itob-constmonth,
    manparno   TYPE bapi_itob-manparno,
    manserno   TYPE bapi_itob-manserno,
    descript   TYPE bapi_itob-descript,
    datab      TYPE bapi_itob-start_from, "Verificar ABAP
    maintplant TYPE bapi_itob-maintplant,
    maintloc   TYPE bapi_itob-maintloc,
    maintroom  TYPE bapi_itob-maintroom, "Verificar ABAP
    plsectn    TYPE bapi_itob-plsectn,
    pp_wkctr   TYPE bapi_itob-pp_wkctr, "Verificar ABAP
    abcindic   TYPE bapi_itob-abcindic,
    sortfield  TYPE bapi_itob-sortfield,
    comp_code  TYPE bapi_itob-comp_code,
    bus_area   TYPE bapi_itob-bus_area,
    asset_no   TYPE bapi_itob-asset_no,
    planplant  TYPE bapi_itob-planplant,
    sub_number TYPE bapi_itob-sub_number,
    costcenter TYPE bapi_itob-costcenter,
    wbs_elem   TYPE bapi_itob-wbs_elem,
    standorder TYPE bapi_itob-standorder,
    settlorder TYPE bapi_itob-settlorder,
    plangroup  TYPE bapi_itob-plangroup,
    work_ctr   TYPE bapi_itob-work_ctr,
    catprofile TYPE bapi_itob-catprofile,
    wergw      TYPE bapi_itob-pp_wkctr,         "Verificar ABAP
    datum      TYPE bapi_itob_parms-inst_date,  "Verificar ABAP
    uzeit      TYPE bapi_itob_parms-inst_time,  "Verificar ABAP
    read_floc  TYPE bapi_itob_eq_only-read_floc,
    consttype  TYPE bapi_itob-consttype,
    inst_pos   TYPE bapi_itob_eq_only-inst_pos,
    techid     TYPE bapi_itob_eq_only-techid,
  END OF registros_cat01.

DATA:
  BEGIN OF registros_cat02,
    external_number  TYPE bapi_itob_parms-equipment,
    valid_date       TYPE bapi_itob_parms-inst_date,
    equicatgry       TYPE bapi_itob_eq_only-equicatgry,
    authgrp          TYPE bapi_itob-authgrp,
    obj_weight       TYPE bapi_itob-obj_weight,
    unit_of_wt       TYPE bapi_itob-unit_of_wt,
    obj_size         TYPE bapi_itob-obj_size,
    inventory        TYPE bapi_itob-inventory,
    start_from       TYPE bapi_itob-start_from,
    objecttype       TYPE bapi_itob-objecttype,
    acquisval        TYPE bapi_itob-acquisval,
    currency         TYPE bapi_itob-currency,
    acqdate          TYPE bapi_itob-acqdate,
    manfacture       TYPE bapi_itob-manfacture,
    mancountry       TYPE bapi_itob-mancountry,
    manmodel         TYPE bapi_itob-manmodel,
    constyear        TYPE bapi_itob-constyear,
    constmonth       TYPE bapi_itob-constmonth,
    manparno         TYPE bapi_itob-manparno,
    manserno         TYPE bapi_itob-manserno,
    descript         TYPE bapi_itob-descript,
    datab            TYPE bapi_itob-start_from, "Verificar ABAP
    maintplant       TYPE bapi_itob-maintplant,
    maintloc         TYPE bapi_itob-maintloc,
    maintroom        TYPE bapi_itob-maintroom, "Verificar ABAP
    plsectn          TYPE bapi_itob-plsectn,
    pp_wkctr         TYPE bapi_itob-pp_wkctr,  "Verificar ABAP
    abcindic         TYPE bapi_itob-abcindic,
    sortfield        TYPE bapi_itob-sortfield,
    comp_code        TYPE bapi_itob-comp_code,
    bus_area         TYPE bapi_itob-bus_area,
    asset_no         TYPE bapi_itob-asset_no,
    costcenter       TYPE bapi_itob-costcenter,
    wbs_elem         TYPE bapi_itob-wbs_elem,
    standorder       TYPE bapi_itob-standorder,
    settlorder       TYPE bapi_itob-settlorder,
    planplant        TYPE bapi_itob-planplant,
    plangroup        TYPE bapi_itob-plangroup,
    work_ctr         TYPE bapi_itob-work_ctr,
    catprofile       TYPE bapi_itob-catprofile,
    read_floc        TYPE bapi_itob_eq_only-read_floc,
    datum            TYPE bapi_itob_parms-inst_date,  "Verificar ABAP
    uzeit            TYPE bapi_itob_parms-inst_time,  "Verificar ABAP
    consttype        TYPE bapi_itob-consttype,
    inst_pos         TYPE bapi_itob_eq_only-inst_pos,
    techid           TYPE bapi_itob_eq_only-techid,
    fleet_num        TYPE bapi_fleet-fleet_num,
    license_num      TYPE bapi_fleet-license_num,
    expiry_date      TYPE bapi_fleet-expiry_date,
    fleet_vin        TYPE bapi_fleet-fleet_vin,
    chassis_num      TYPE bapi_fleet-chassis_num,
    gross_wgt        TYPE bapi_fleet-gross_wgt,
    load_wgt         TYPE bapi_fleet-load_wgt,
    load_vol         TYPE bapi_fleet-load_vol,
    vol_unit         TYPE bapi_fleet-vol_unit,
    load_hgt         TYPE bapi_fleet-load_hgt,
    load_dim_unit    TYPE bapi_fleet-load_dim_unit,
    load_wid         TYPE bapi_fleet-load_wid,
    load_len         TYPE bapi_fleet-load_len,
    no_compart       TYPE bapi_fleet-no_compart,
    fleet_hgt        TYPE bapi_fleet-fleet_hgt,
    dim_unit         TYPE bapi_fleet-dim_unit,
    fleet_wid        TYPE bapi_fleet-fleet_wid,
    fleet_len        TYPE bapi_fleet-fleet_len,
    repla_date       TYPE bapi_fleet-repla_date,
    repla_odom       TYPE bapi_fleet-repla_odom,
    repla_oph        TYPE bapi_fleet-repla_oph,
    fleet_use        TYPE bapi_fleet-fleet_use,
    card_num         TYPE bapi_fleet-card_num,
    max_occupants    TYPE bapi_fleet-max_occupants,
    key_num          TYPE bapi_fleet-key_num,
    num_axle         TYPE bapi_fleet-num_axle,
    engine_type      TYPE bapi_fleet-engine_type,
    engine_snr       TYPE bapi_fleet-engine_snr,
    speed_max        TYPE bapi_fleet-speed_max,
    speed_unit       TYPE bapi_fleet-speed_unit,
    engine_power     TYPE bapi_fleet-engine_power,
    unit_power       TYPE bapi_fleet-unit_power,
    revolutions      TYPE bapi_fleet-revolutions,
    engine_cap       TYPE bapi_fleet-engine_cap,
    unit_cap         TYPE bapi_fleet-unit_cap,
    engine_cyl       TYPE bapi_fleet-engine_cyl,
    fuel_pri         TYPE bapi_fleet-fuel_pri,
    fuel_sec         TYPE bapi_fleet-fuel_sec,
    oil_type         TYPE bapi_fleet-oil_type,
    pri_calc         TYPE bapi_fleet-pri_calc,
    div1             TYPE fleet-div1, "(Append tabela Fleet)
    tq_combustivel_1 TYPE fleet-tq_combustivel_1,  "(Append tabela Fleet)
    div2             TYPE fleet-div2,  "(Append tabela Fleet)
    tq_combustivel_2 TYPE fleet-tq_combustivel_2,  "(Append tabela Fleet)
    div3             TYPE fleet-div3, "(Append tabela Fleet)
    tq_combustivel_3 TYPE fleet-tq_combustivel_3,  "(Append tabela Fleet)
    flag             TYPE c,
  END OF registros_cat02.

TYPES: BEGIN OF ty_saida_cat01_m,
         id          TYPE p,
         status      TYPE char05,
         desc_status TYPE char50,
         equipment   TYPE bapi_itob_parms-equipment,
         valid_date  TYPE bapi_itob_parms-inst_date,
         equicatgry  TYPE bapi_itob_eq_only-equicatgry,
         authgrp     TYPE bapi_itob-authgrp,
         obj_weight  TYPE bapi_itob-obj_weight,
         unit_of_wt  TYPE bapi_itob-unit_of_wt,
         obj_size    TYPE bapi_itob-obj_size,
         inventory   TYPE bapi_itob-inventory,
         start_from  TYPE bapi_itob-start_from,
         objecttype  TYPE bapi_itob-objecttype,
         acquisval   TYPE bapi_itob-acquisval,
         currency    TYPE bapi_itob-currency,
         acqdate     TYPE bapi_itob-acqdate,
         manfacture  TYPE bapi_itob-manfacture,
         mancountry  TYPE bapi_itob-mancountry,
         manmodel    TYPE bapi_itob-manmodel,
         constyear   TYPE bapi_itob-constyear,
         constmonth  TYPE bapi_itob-constmonth,
         manparno    TYPE bapi_itob-manparno,
         manserno    TYPE bapi_itob-manserno,
         descript    TYPE bapi_itob-descript,
         datab       TYPE bapi_itob-start_from, "Verificar ABAP
         maintplant  TYPE bapi_itob-maintplant,
         maintloc    TYPE bapi_itob-maintloc,
         maintroom   TYPE bapi_itob-maintroom, "Verificar ABAP
         plsectn     TYPE bapi_itob-plsectn,
         pp_wkctr    TYPE bapi_itob-pp_wkctr,  "Verificar ABAP
         abcindic    TYPE bapi_itob-abcindic,
         sortfield   TYPE bapi_itob-sortfield,
         comp_code   TYPE bapi_itob-comp_code,
         bus_area    TYPE bapi_itob-bus_area,
         asset_no    TYPE bapi_itob-asset_no,
         planplant   TYPE bapi_itob-planplant,
         sub_number  TYPE bapi_itob-sub_number,
         costcenter  TYPE bapi_itob-costcenter,
         wbs_elem    TYPE bapi_itob-wbs_elem,
         standorder  TYPE bapi_itob-standorder,
         settlorder  TYPE bapi_itob-settlorder,
         plangroup   TYPE bapi_itob-plangroup,
         work_ctr    TYPE bapi_itob-work_ctr,
         catprofile  TYPE bapi_itob-catprofile,
         wergw       TYPE bapi_itob-pp_wkctr,        "Verificar abap
         datum       TYPE bapi_itob_parms-inst_date, "Verificar abap
         uzeit       TYPE bapi_itob_parms-inst_time, "Verificar abap
         read_floc   TYPE bapi_itob_eq_only-read_floc,
         consttype   TYPE bapi_itob-consttype,
         inst_pos    TYPE bapi_itob_eq_only-inst_pos,
         techid      TYPE bapi_itob_eq_only-techid,
         flag        TYPE c,
       END OF ty_saida_cat01_m.
DATA:
  BEGIN OF registros_cat01_m OCCURS 1,
    equipment  TYPE bapi_itob_parms-equipment,
    valid_date TYPE bapi_itob_parms-inst_date,
    equicatgry TYPE bapi_itob_eq_only-equicatgry,
    authgrp    TYPE bapi_itob-authgrp,
    obj_weight TYPE bapi_itob-obj_weight,
    unit_of_wt TYPE bapi_itob-unit_of_wt,
    obj_size   TYPE bapi_itob-obj_size,
    inventory  TYPE bapi_itob-inventory,
    start_from TYPE bapi_itob-start_from,
    objecttype TYPE bapi_itob-objecttype,
    acquisval  TYPE bapi_itob-acquisval,
    currency   TYPE bapi_itob-currency,
    acqdate    TYPE bapi_itob-acqdate,
    manfacture TYPE bapi_itob-manfacture,
    mancountry TYPE bapi_itob-mancountry,
    manmodel   TYPE bapi_itob-manmodel,
    constyear  TYPE bapi_itob-constyear,
    constmonth TYPE bapi_itob-constmonth,
    manparno   TYPE bapi_itob-manparno,
    manserno   TYPE bapi_itob-manserno,
    descript   TYPE bapi_itob-descript,
    datab      TYPE bapi_itob-start_from,
    maintplant TYPE bapi_itob-maintplant,
    maintloc   TYPE bapi_itob-maintloc,
    maintroom  TYPE bapi_itob-maintroom,
    plsectn    TYPE bapi_itob-plsectn,
    pp_wkctr   TYPE bapi_itob-pp_wkctr,
    abcindic   TYPE bapi_itob-abcindic,
    sortfield  TYPE bapi_itob-sortfield,
    comp_code  TYPE bapi_itob-comp_code,
    bus_area   TYPE bapi_itob-bus_area,
    asset_no   TYPE bapi_itob-asset_no,
    planplant  TYPE bapi_itob-planplant,
    sub_number TYPE bapi_itob-sub_number,
    costcenter TYPE bapi_itob-costcenter,
    wbs_elem   TYPE bapi_itob-wbs_elem,
    standorder TYPE bapi_itob-standorder,
    settlorder TYPE bapi_itob-settlorder,
    plangroup  TYPE bapi_itob-plangroup,
    work_ctr   TYPE bapi_itob-work_ctr,
    catprofile TYPE bapi_itob-catprofile,
    wergw      TYPE bapi_itob-pp_wkctr,
    datum      TYPE bapi_itob_parms-inst_date,
    uzeit      TYPE bapi_itob_parms-inst_time,
    read_floc  TYPE bapi_itob_eq_only-read_floc,
    consttype  TYPE bapi_itob-consttype,
    inst_pos   TYPE bapi_itob_eq_only-inst_pos,
    techid     TYPE bapi_itob_eq_only-techid,
  END OF registros_cat01_m.

TYPES: BEGIN OF ty_saida_cat02_m,
         id               TYPE p,
         status           TYPE char05,
         desc_status      TYPE char50,
         equipment        TYPE bapi_itob_parms-equipment,
         valid_date       TYPE bapi_itob_parms-inst_date,
         equicatgry       TYPE bapi_itob_eq_only-equicatgry,
         authgrp          TYPE bapi_itob-authgrp,
         obj_weight       TYPE bapi_itob-obj_weight,
         unit_of_wt       TYPE bapi_itob-unit_of_wt,
         obj_size         TYPE bapi_itob-obj_size,
         inventory        TYPE bapi_itob-inventory,
         start_from       TYPE bapi_itob-start_from,
         objecttype       TYPE bapi_itob-objecttype,
         acquisval        TYPE bapi_itob-acquisval,
         currency         TYPE bapi_itob-currency,
         acqdate          TYPE bapi_itob-acqdate,
         manfacture       TYPE bapi_itob-manfacture,
         mancountry       TYPE bapi_itob-mancountry,
         manmodel         TYPE bapi_itob-manmodel,
         constyear        TYPE bapi_itob-constyear,
         constmonth       TYPE bapi_itob-constmonth,
         manparno         TYPE bapi_itob-manparno,
         manserno         TYPE bapi_itob-manserno,
         descript         TYPE bapi_itob-descript,
         datab            TYPE bapi_itob-start_from, "Verificar ABAP
         maintplant       TYPE bapi_itob-maintplant,
         maintloc         TYPE bapi_itob-maintloc,
         maintroom        TYPE bapi_itob-maintroom,  "Verificar ABAP
         plsectn          TYPE bapi_itob-plsectn,
         pp_wkctr         TYPE bapi_itob-pp_wkctr,  "Verificar ABAP
         abcindic         TYPE bapi_itob-abcindic,
         sortfield        TYPE bapi_itob-sortfield,
         comp_code        TYPE bapi_itob-comp_code,
         bus_area         TYPE bapi_itob-bus_area,
         asset_no         TYPE bapi_itob-asset_no,
         costcenter       TYPE bapi_itob-costcenter,
         wbs_elem         TYPE bapi_itob-wbs_elem,
         standorder       TYPE bapi_itob-standorder,
         settlorder       TYPE bapi_itob-settlorder,
         planplant        TYPE bapi_itob-planplant,
         plangroup        TYPE bapi_itob-plangroup,
         work_ctr         TYPE bapi_itob-work_ctr,
         catprofile       TYPE bapi_itob-catprofile,
         read_floc        TYPE bapi_itob_eq_only-read_floc,
         datum            TYPE bapi_itob_parms-inst_date, "Verificar abap
         uzeit            TYPE bapi_itob_parms-inst_time, "Verificar abap
         consttype        TYPE bapi_itob-consttype,
         inst_pos         TYPE bapi_itob_eq_only-inst_pos,
         techid           TYPE bapi_itob_eq_only-techid,
         fleet_num        TYPE bapi_fleet-fleet_num,
         license_num      TYPE bapi_fleet-license_num,
         expiry_date      TYPE bapi_fleet-expiry_date,
         fleet_vin        TYPE bapi_fleet-fleet_vin,
         chassis_num      TYPE bapi_fleet-chassis_num,
         gross_wgt        TYPE bapi_fleet-gross_wgt,
         load_wgt         TYPE bapi_fleet-load_wgt,
         load_vol         TYPE bapi_fleet-load_vol,
         vol_unit         TYPE bapi_fleet-vol_unit,
         load_hgt         TYPE bapi_fleet-load_hgt,
         load_dim_unit    TYPE bapi_fleet-load_dim_unit,
         load_wid         TYPE bapi_fleet-load_wid,
         load_len         TYPE bapi_fleet-load_len,
         no_compart       TYPE bapi_fleet-no_compart,
         fleet_hgt        TYPE bapi_fleet-fleet_hgt,
         dim_unit         TYPE bapi_fleet-dim_unit,
         fleet_wid        TYPE bapi_fleet-fleet_wid,
         fleet_len        TYPE bapi_fleet-fleet_len,
         repla_date       TYPE bapi_fleet-repla_date,
         repla_odom       TYPE bapi_fleet-repla_odom,
         repla_oph        TYPE bapi_fleet-repla_oph,
         fleet_use        TYPE bapi_fleet-fleet_use,
         card_num         TYPE bapi_fleet-card_num,
         max_occupants    TYPE bapi_fleet-max_occupants,
         key_num          TYPE bapi_fleet-key_num,
         num_axle         TYPE bapi_fleet-num_axle,
         engine_type      TYPE bapi_fleet-engine_type,
         engine_snr       TYPE bapi_fleet-engine_snr,
         speed_max        TYPE bapi_fleet-speed_max,
         speed_unit       TYPE bapi_fleet-speed_unit,
         engine_power     TYPE bapi_fleet-engine_power,
         unit_power       TYPE bapi_fleet-unit_power,
         revolutions      TYPE bapi_fleet-revolutions,
         engine_cap       TYPE bapi_fleet-engine_cap,
         unit_cap         TYPE bapi_fleet-unit_cap,
         engine_cyl       TYPE bapi_fleet-engine_cyl,
         fuel_pri         TYPE bapi_fleet-fuel_pri,
         fuel_sec         TYPE bapi_fleet-fuel_sec,
         oil_type         TYPE bapi_fleet-oil_type,
         pri_calc         TYPE bapi_fleet-pri_calc,
         div1             TYPE fleet-div1, "(Append tabela Fleet)
         tq_combustivel_1 TYPE fleet-tq_combustivel_1,  "(Append tabela Fleet)
         div2             TYPE fleet-div2,  "(Append tabela Fleet)
         tq_combustivel_2 TYPE fleet-tq_combustivel_2,  "(Append tabela Fleet)
         div3             TYPE fleet-div3, "(Append tabela Fleet)
         tq_combustivel_3 TYPE fleet-tq_combustivel_3,  "(Append tabela Fleet)
         flag             TYPE c,
       END OF ty_saida_cat02_m.
DATA:
  BEGIN OF registros_cat02_m,
    equipment        TYPE bapi_itob_parms-equipment,
    valid_date       TYPE bapi_itob_parms-inst_date,
    equicatgry       TYPE bapi_itob_eq_only-equicatgry,
    authgrp          TYPE bapi_itob-authgrp,
    obj_weight       TYPE bapi_itob-obj_weight,
    unit_of_wt       TYPE bapi_itob-unit_of_wt,
    obj_size         TYPE bapi_itob-obj_size,
    inventory        TYPE bapi_itob-inventory,
    start_from       TYPE bapi_itob-start_from,
    objecttype       TYPE bapi_itob-objecttype,
    acquisval        TYPE bapi_itob-acquisval,
    currency         TYPE bapi_itob-currency,
    acqdate          TYPE bapi_itob-acqdate,
    manfacture       TYPE bapi_itob-manfacture,
    mancountry       TYPE bapi_itob-mancountry,
    manmodel         TYPE bapi_itob-manmodel,
    constyear        TYPE bapi_itob-constyear,
    constmonth       TYPE bapi_itob-constmonth,
    manparno         TYPE bapi_itob-manparno,
    manserno         TYPE bapi_itob-manserno,
    descript         TYPE bapi_itob-descript,
    datab            TYPE bapi_itob-start_from, "Verificar ABAP
    maintplant       TYPE bapi_itob-maintplant,
    maintloc         TYPE bapi_itob-maintloc,
    maintroom        TYPE bapi_itob-maintroom, "Verificar ABAP
    plsectn          TYPE bapi_itob-plsectn,
    pp_wkctr         TYPE bapi_itob-pp_wkctr,  "Verificar ABAP
    abcindic         TYPE bapi_itob-abcindic,
    sortfield        TYPE bapi_itob-sortfield,
    comp_code        TYPE bapi_itob-comp_code,
    bus_area         TYPE bapi_itob-bus_area,
    asset_no         TYPE bapi_itob-asset_no,
    costcenter       TYPE bapi_itob-costcenter,
    wbs_elem         TYPE bapi_itob-wbs_elem,
    standorder       TYPE bapi_itob-standorder,
    settlorder       TYPE bapi_itob-settlorder,
    planplant        TYPE bapi_itob-planplant,
    plangroup        TYPE bapi_itob-plangroup,
    work_ctr         TYPE bapi_itob-work_ctr,
    catprofile       TYPE bapi_itob-catprofile,
    read_floc        TYPE bapi_itob_eq_only-read_floc,
    datum            TYPE bapi_itob_parms-inst_date,  "Verificar ABAP
    uzeit            TYPE bapi_itob_parms-inst_time,  "Verificar ABAP
    consttype        TYPE bapi_itob-consttype,
    inst_pos         TYPE bapi_itob_eq_only-inst_pos,
    techid           TYPE bapi_itob_eq_only-techid,
    fleet_num        TYPE bapi_fleet-fleet_num,
    license_num      TYPE bapi_fleet-license_num,
    expiry_date      TYPE bapi_fleet-expiry_date,
    fleet_vin        TYPE bapi_fleet-fleet_vin,
    chassis_num      TYPE bapi_fleet-chassis_num,
    gross_wgt        TYPE bapi_fleet-gross_wgt,
    load_wgt         TYPE bapi_fleet-load_wgt,
    load_vol         TYPE bapi_fleet-load_vol,
    vol_unit         TYPE bapi_fleet-vol_unit,
    load_hgt         TYPE bapi_fleet-load_hgt,
    load_dim_unit    TYPE bapi_fleet-load_dim_unit,
    load_wid         TYPE bapi_fleet-load_wid,
    load_len         TYPE bapi_fleet-load_len,
    no_compart       TYPE bapi_fleet-no_compart,
    fleet_hgt        TYPE bapi_fleet-fleet_hgt,
    dim_unit         TYPE bapi_fleet-dim_unit,
    fleet_wid        TYPE bapi_fleet-fleet_wid,
    fleet_len        TYPE bapi_fleet-fleet_len,
    repla_date       TYPE bapi_fleet-repla_date,
    repla_odom       TYPE bapi_fleet-repla_odom,
    repla_oph        TYPE bapi_fleet-repla_oph,
    fleet_use        TYPE bapi_fleet-fleet_use,
    card_num         TYPE bapi_fleet-card_num,
    max_occupants    TYPE bapi_fleet-max_occupants,
    key_num          TYPE bapi_fleet-key_num,
    num_axle         TYPE bapi_fleet-num_axle,
    engine_type      TYPE bapi_fleet-engine_type,
    engine_snr       TYPE bapi_fleet-engine_snr,
    speed_max        TYPE bapi_fleet-speed_max,
    speed_unit       TYPE bapi_fleet-speed_unit,
    engine_power     TYPE bapi_fleet-engine_power,
    unit_power       TYPE bapi_fleet-unit_power,
    revolutions      TYPE bapi_fleet-revolutions,
    engine_cap       TYPE bapi_fleet-engine_cap,
    unit_cap         TYPE bapi_fleet-unit_cap,
    engine_cyl       TYPE bapi_fleet-engine_cyl,
    fuel_pri         TYPE bapi_fleet-fuel_pri,
    fuel_sec         TYPE bapi_fleet-fuel_sec,
    oil_type         TYPE bapi_fleet-oil_type,
    pri_calc         TYPE bapi_fleet-pri_calc,
    div1             TYPE fleet-div1, "(Append tabela Fleet)
    tq_combustivel_1 TYPE fleet-tq_combustivel_1,  "(Append tabela Fleet)
    div2             TYPE fleet-div2,  "(Append tabela Fleet)
    tq_combustivel_2 TYPE fleet-tq_combustivel_2,  "(Append tabela Fleet)
    div3             TYPE fleet-div3, "(Append tabela Fleet)
    tq_combustivel_3 TYPE fleet-tq_combustivel_3,  "(Append tabela Fleet)
    flag             TYPE c,
  END OF registros_cat02_m.

TYPES: BEGIN OF ty_erros,
         linha      TYPE sy-index,
         id         TYPE bapiret2-id,
         type       TYPE bapiret2-type,
         number     TYPE bapiret2-number,
         message    TYPE bapiret2-message,
         message_v1 TYPE bapiret2-message_v1,
         message_v2 TYPE bapiret2-message_v2,
         message_v3 TYPE bapiret2-message_v3,
       END OF ty_erros.


TYPES: BEGIN OF ty_ucomm,
         ucomm TYPE  sy-ucomm,
       END OF ty_ucomm.

TYPES: BEGIN OF ty_status,
         id          TYPE p,
         status      TYPE  char05,
         desc_status TYPE bapi_msg,
         type        TYPE char01,
       END OF ty_status.

"Tabela Interna Global
DATA: git_saida_cat01          TYPE TABLE OF ty_saida_cat01   WITH HEADER LINE,
      git_saida_cat02          TYPE TABLE OF ty_saida_cat02   WITH HEADER LINE,
      git_saida_cat01_m        TYPE TABLE OF ty_saida_cat01_m WITH HEADER LINE,
      git_saida_cat02_m        TYPE TABLE OF ty_saida_cat02_m WITH HEADER LINE,

      git_erros                TYPE TABLE OF ty_erros WITH HEADER LINE,
      git_filtro               TYPE zif_screen_linha_filtro_t,
      editor                   TYPE REF TO cl_gui_textedit,
      c_editor                 TYPE REF TO cl_gui_custom_container,

      t_status                 TYPE TABLE OF ty_status WITH HEADER LINE,
      git_saida_local_inst_001 TYPE STANDARD TABLE OF ty_saida_local_inst_001,
      git_saida_local_inst_002 TYPE STANDARD TABLE OF ty_saida_local_inst_002.

"Objetos
DATA: gob_custom_container        TYPE REF TO cl_gui_custom_container,
      gob_dd_document             TYPE REF TO cl_dd_document,
      gob_splitter_container_main TYPE REF TO cl_gui_splitter_container,
      gob_splitter_container_topo TYPE REF TO cl_gui_splitter_container,

      gob_gui_container_topo      TYPE REF TO cl_gui_container,
      gob_gui_container_filtro    TYPE REF TO cl_gui_container,
      gob_gui_container_logo      TYPE REF TO cl_gui_container,
      gob_gui_container_grid      TYPE REF TO cl_gui_container,
      gob_gui_picture             TYPE REF TO cl_gui_picture,
      git_fcat_cat01              TYPE lvc_t_fcat,
      git_fcat_cat02              TYPE lvc_t_fcat,
      git_fcat_cat01_m            TYPE lvc_t_fcat,
      git_fcat_cat02_m            TYPE lvc_t_fcat,
      git_fcat_proc               TYPE lvc_t_fcat,
      git_local_inst_001          TYPE lvc_t_fcat,
      git_local_inst_002          TYPE lvc_t_fcat,
      gob_gui_alv_grid            TYPE REF TO cl_gui_alv_grid,
      ls_stable                   TYPE lvc_s_stbl.

DATA: gva_cat             TYPE string,
      gva_arq             LIKE rlgrap-filename,
      git_dados           LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE,
      gva_valid_date      LIKE bapi_itob_parms-inst_date,
      gva_external_number TYPE bapi_itob_parms-equipment,
      gva_equipment       TYPE bapi_itob_parms-equipment,
      gva_verificado      TYPE c,
      local               TYPE bapi_eqhr-funcloc,
      gva_erro            TYPE c.

*DATA: it_ucomm TYPE TABLE OF ty_ucomm.
DATA: fcode  TYPE TABLE OF ty_ucomm,
      p_erro TYPE char03.

DEFINE m_message.
  CASE sy-subrc.
    WHEN 0.
    WHEN 1.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    WHEN OTHERS.
  ENDCASE.
END-OF-DEFINITION.

TYPES: BEGIN OF ty_alsmex_tabline,
         row   TYPE kcd_ex_row_n,
         col   TYPE kcd_ex_col_n,
         value TYPE char8000,
       END OF ty_alsmex_tabline.

TYPES: BEGIN OF ty_s_senderline,
         line(5120) TYPE c,
       END OF ty_s_senderline,
       ty_t_sender TYPE ty_s_senderline.

DATA: excel_tab    TYPE TABLE OF ty_t_sender.
DATA: it_dados TYPE TABLE OF char8000.

DATA: ld_separator TYPE c,
      application  TYPE ole2_object,
      workbook     TYPE ole2_object,
      range        TYPE ole2_object,
      worksheet    TYPE ole2_object,
      h_cell       TYPE ole2_object,
      h_cell1      TYPE ole2_object,
      ld_rc        TYPE i.



DATA:
  it_index TYPE lvc_t_row,
  it_excel TYPE TABLE OF ty_alsmex_tabline,
  it_aux   TYPE TABLE OF ty_alsmex_tabline WITH DEFAULT KEY,
  cont_col TYPE kcd_ex_col_n,
  t_excel  TYPE hrcnex_tab.

DATA: icon_proc TYPE string.

*** Anexos:
DATA: t_anexos     TYPE TABLE OF bdn_con,
      l_obj_key    TYPE sibflporb-instid,
      l_lines      TYPE i,
      anexo_obj    TYPE REF TO cl_gos_manager,
      l_ip_mode    TYPE sgs_rwmod,
      l_ip_service TYPE sgs_srvnam,
      w_bor        TYPE borident.

"Field-Symbol
FIELD-SYMBOLS: <gfs_t001> TYPE t001.

DATA: c_empresa TYPE bukrs,
      c_centro  TYPE werks_d.

" Classe
CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA:  event_receiver   TYPE REF TO lcl_event_receiver.

CLASS zcl_instrucao DEFINITION.

  PUBLIC SECTION.
    METHODS:

      get_excel
        RETURNING VALUE(r_value) TYPE rlgrap-filename,

      set_excel
        IMPORTING input TYPE char128,
*      exc_excel,

      col RETURNING VALUE(r_value) TYPE kcd_ex_col_n.
ENDCLASS.

DATA(obj_inst) = NEW zcl_instrucao( ).

CLASS zcl_instrucao IMPLEMENTATION.

  METHOD get_excel.

    DATA: it_file TYPE filetable,
          l_subrc TYPE i.

    cl_gui_frontend_services=>file_open_dialog(  EXPORTING default_filename = ' '
                                                           file_filter      = '*.xls'
                                                 CHANGING  file_table       = it_file
                                                           rc               = l_subrc
                                               ).
    TRY .
        r_value = it_file[ 1 ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR r_value.
    ENDTRY.
  ENDMETHOD.

  METHOD set_excel.

    CHECK input IS NOT INITIAL.

    FREE: it_excel[], excel_tab[].

    CLASS cl_abap_char_utilities DEFINITION LOAD.

    ld_separator = cl_abap_char_utilities=>horizontal_tab.

    IF application-header = space OR application-handle = -1.
      CREATE OBJECT application 'Excel.Application'.
      m_message.
    ENDIF.

    CALL METHOD OF application 'Workbooks' = workbook.
    m_message.
    CALL METHOD OF workbook 'Open' EXPORTING #1 = input.
    m_message.
    GET PROPERTY OF  application 'ACTIVESHEET' = worksheet.
    m_message.
    CALL METHOD OF worksheet 'Cells' = h_cell
        EXPORTING #1 = 1 #2 = 1.
    m_message.
    CALL METHOD OF worksheet 'Cells' = h_cell1
        EXPORTING #1 = 10000 #2 = 95.
    m_message.

    CALL METHOD  OF worksheet 'RANGE' = range
                   EXPORTING #1 = h_cell #2 = h_cell1.
    m_message.
    CALL METHOD OF range 'SELECT'.
    m_message.
    CALL METHOD OF range 'COPY'.
    m_message.

    cl_gui_frontend_services=>clipboard_import( IMPORTING data = excel_tab ).
    IF sy-subrc <> 0.
      MESSAGE a037(alsmex).
    ENDIF.

    LOOP AT excel_tab INTO DATA(wa).

      DATA(cont) = sy-tabix.
      SPLIT wa AT ld_separator INTO TABLE it_dados.
      it_aux = VALUE #( FOR ls IN it_dados (
                         row   = cont
                         col   = col( )
                         value = ls
                        ) ).
      CLEAR cont_col.
      APPEND LINES OF it_aux TO it_excel.
    ENDLOOP.

    FREE excel_tab.
    cl_gui_frontend_services=>clipboard_export(  IMPORTING data = excel_tab
                                                 CHANGING  rc   = ld_rc
                                               ).

    CALL METHOD OF application 'QUIT'.
    m_message.

    FREE OBJECT h_cell.       m_message.
    FREE OBJECT h_cell1.      m_message.
    FREE OBJECT range.        m_message.
    FREE OBJECT worksheet.    m_message.
    FREE OBJECT workbook.     m_message.
    FREE OBJECT application.  m_message.
  ENDMETHOD.

  METHOD col.
    ADD 1 TO cont_col.
    r_value = cont_col.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:

      hotspot_click
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no .


    CLASS-METHODS
      handle_double_click
        FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD hotspot_click.
    CASE e_column_id.
      WHEN ''.
    ENDCASE.

    CALL METHOD gob_gui_alv_grid->refresh_table_display.

  ENDMETHOD.

  METHOD handle_double_click.

    CHECK e_row-rowtype(1) EQ space.
    PERFORM sel_status  USING e_row e_column-fieldname.

  ENDMETHOD. " HANDLE_DOUBLE_CLICK
ENDCLASS.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECTION-SCREEN COMMENT /1(50) TEXT-002.

  PARAMETERS: p_linst  RADIOBUTTON GROUP rb1 USER-COMMAND sel.

  SELECTION-SCREEN SKIP 1.

  PARAMETERS: p_tplkz LIKE t370s-tplkz."        MATCHCODE OBJECT id.. " Cód.estrutura
  PARAMETERS: p_fltyp TYPE t370f-fltyp.         "Ctg.loc.instl.
  PARAMETERS: p_tipo  TYPE ze_tipo_equipamento. "Tipo equipamento

  SELECTION-SCREEN SKIP 1.

  PARAMETERS: p_linstm RADIOBUTTON GROUP rb1.

  SELECTION-SCREEN SKIP 1.

*SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-004.
  SELECTION-SCREEN COMMENT /1(50) TEXT-003.
  PARAMETERS: p_equipc  RADIOBUTTON  GROUP rb1. "USER-COMMAND sel.

  PARAMETERS: p_eqtyp TYPE t370t-eqtyp. "Ctg.equipamento Criar

  PARAMETERS: p_equipm  RADIOBUTTON GROUP rb1.
  PARAMETERS: p_eqtypm TYPE t370t-eqtyp. "Ctg.equipamento Modificar
*SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN FUNCTION KEY 1.

INITIALIZATION.

  icon_proc = icon_attachment && 'DOWNLOAD PLANILHA PADRÃO CARGA'.
  sscrfields-functxt_01 = icon_proc .

AT SELECTION-SCREEN. "PAI
  CASE sscrfields-ucomm. "pushbutton pressed
    WHEN 'FC01'.
      PERFORM f_importar_anexos.
  ENDCASE.


*SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
*
*PARAMETERS: p_equipc  RADIOBUTTON  GROUP rb2. "USER-COMMAND sel.
*
*PARAMETERS: p_eqtyp TYPE t370t-eqtyp. "Ctg.equipamento Criar
*
*PARAMETERS: p_equipm  RADIOBUTTON GROUP rb2.
*PARAMETERS: p_eqtypm TYPE t370t-eqtyp. "Ctg.equipamento Modificar
*
*SELECTION-SCREEN: END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  PERFORM fm_at_selection_screen.

START-OF-SELECTION.
  PERFORM fm_start_of_selection.

END-OF-SELECTION.
  PERFORM fm_end_of_selection.


*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_start_of_selection .

  PERFORM fm_dados_seleciona.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FM_END_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_end_of_selection .
  IF p_linst IS NOT INITIAL AND p_tplkz IS INITIAL AND p_fltyp IS INITIAL.
    "Envia message de processamento.
    MESSAGE i000(z01) WITH 'Preencha o cód.estrutura e '
                           'ctg.loc.instalação.'.

    EXIT.
  ENDIF.

  IF p_equipc IS NOT INITIAL AND p_eqtyp IS INITIAL.
    "Envia message de processamento.
    MESSAGE i000(z01) WITH 'Preencha a ctg.equipamento '.
    EXIT.
  ENDIF.

  IF p_equipm IS NOT INITIAL AND p_eqtypm IS INITIAL.
    "Envia message de processamento.
    MESSAGE i000(z01) WITH 'Preencha a Ctg.equipamento '.
  ENDIF.

  PERFORM fm_filtros.
  CALL SCREEN 0100.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_seleciona .
  IF p_equipc IS NOT INITIAL OR p_equipm IS NOT INITIAL.
    " Indpustria, TI, Oficinas, Pneus, Agregados.
    IF p_eqtyp EQ 'E'
      OR p_eqtypm  EQ 'E'
      OR p_eqtyp EQ 'I' OR p_eqtypm  EQ 'I'
      OR p_eqtyp EQ 'M' OR p_eqtypm  EQ 'M'
      OR p_eqtyp EQ 'T' OR p_eqtypm  EQ 'T'
      OR p_eqtyp EQ 'U' OR p_eqtypm  EQ 'U'.
      gva_cat = '01'.
    ELSE.
      " Frota
      IF p_eqtyp EQ 'A'   OR p_eqtypm  EQ 'A'
       OR  p_eqtyp EQ 'F' OR p_eqtypm  EQ 'F'
       OR  p_eqtyp EQ 'V' OR p_eqtypm  EQ 'V' OR  p_eqtyp EQ 'D' OR p_eqtypm  EQ 'D'.
        gva_cat = '02'.
      ENDIF.
    ENDIF.
  ELSE.

  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_at_selection_screen .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  PERFORM fm_mod_state_bottom.

  SET PF-STATUS 'PF0100' EXCLUDING fcode.
  SET TITLEBAR  'TB0100' WITH 'COCKPIT - CARGA PM LOCAL DE INSTALAÇÃO & EQUIPAMENTO'.

  PERFORM fm_criar_objetos.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'IMP_DATA'.
      PERFORM fm_dados_importa.
    WHEN 'EXECUTAR'.
      PERFORM fm_dados_executar.
    WHEN 'VER_INFO'.
      PERFORM fm_verificar_inform.
    WHEN 'LIMP_ALV'.
      PERFORM fm_limpar_alv.
    WHEN 'ELIM_LINHA'.
      PERFORM fm_elim_linha.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FM_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_criar_objetos.

  DATA: l_alv_local_instalacao TYPE xflag.

  DATA: lva_data(22)  TYPE c,
        lva_status_op TYPE string.

  DATA: lwa_layout     TYPE lvc_s_layo.

  lwa_layout-zebra      = 'X'.
  lwa_layout-sel_mode   = 'A'.

  PERFORM fm_cria_fieldcat.

  IF p_equipc = 'X'.
    lva_status_op = 'Criar Equipamento'.
  ELSE.
    IF p_equipm = 'X'.
      lva_status_op = 'Modificar Equipamento'.
    ENDIF.
    IF p_linst = 'X'.
      lva_status_op = 'Criar local de instalação'.
    ENDIF.
    IF p_linstm = 'X'.
      lva_status_op = 'Modificar local de instalação'.
    ENDIF.
  ENDIF.

  CONCATENATE sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) INTO lva_data.

*  IF t_status[] IS INITIAL AND  git_fcat_cat01 IS NOT INITIAL.
*    DELETE git_fcat_cat01 WHERE fieldname = 'STATUS'.
*  ENDIF.
*
*  IF t_status[] IS INITIAL AND  git_fcat_cat02 IS NOT INITIAL.
*    DELETE git_fcat_cat02 WHERE fieldname = 'STATUS'.
*  ENDIF.
*
*  IF t_status[] IS INITIAL AND  git_fcat_cat01_m IS NOT INITIAL..
*    DELETE git_fcat_cat01_m WHERE fieldname = 'STATUS'.
*  ENDIF.
*
*  IF t_status[] IS INITIAL AND  git_fcat_cat02_m IS NOT INITIAL..
*    DELETE git_fcat_cat02_m WHERE fieldname = 'STATUS'.
*  ENDIF.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
  EXPORTING
     i_titulo  = lva_status_op
     i_filtros = VALUE zif_screen_linha_filtro_t( ( parametro = 'Data Posição' valor = lva_data ) )
   CHANGING
     alv = gob_gui_alv_grid
   )
   EQ abap_true.


    gob_gui_alv_grid->activate_display_protocol( 'X' ).
    SET HANDLER: lcl_event_receiver=>handle_double_click FOR gob_gui_alv_grid.

    lwa_layout-col_opt = 'X'.
    lwa_layout-cwidth_opt = 'X'.
    lwa_layout-zebra = 'X'.


    CLEAR: l_alv_local_instalacao.
    IF p_linst = 'X' OR p_linstm EQ 'X'.
      CASE p_tipo.
        WHEN '0001'.
          CALL METHOD gob_gui_alv_grid->set_table_for_first_display
            EXPORTING
              is_layout                     = lwa_layout
            CHANGING
              it_outtab                     = git_saida_local_inst_001[]
              it_fieldcatalog               = git_local_inst_001
            EXCEPTIONS
              invalid_parameter_combination = 1
              program_error                 = 2
              too_many_lines                = 3
              OTHERS                        = 4.
          l_alv_local_instalacao = 'X'.
        WHEN '0002'.
          CALL METHOD gob_gui_alv_grid->set_table_for_first_display
            EXPORTING
              is_layout                     = lwa_layout
            CHANGING
              it_outtab                     = git_saida_local_inst_002[]
              it_fieldcatalog               = git_local_inst_002
            EXCEPTIONS
              invalid_parameter_combination = 1
              program_error                 = 2
              too_many_lines                = 3
              OTHERS                        = 4.
          l_alv_local_instalacao = 'X'.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    IF l_alv_local_instalacao IS INITIAL.
      IF p_equipc = 'X'.
        IF gva_cat = '01'.
          CALL METHOD gob_gui_alv_grid->set_table_for_first_display
            EXPORTING
              is_layout                     = lwa_layout
            CHANGING
              it_outtab                     = git_saida_cat01[]
              it_fieldcatalog               = git_fcat_cat01
            EXCEPTIONS
              invalid_parameter_combination = 1
              program_error                 = 2
              too_many_lines                = 3
              OTHERS                        = 4.
        ELSE.

          CALL METHOD gob_gui_alv_grid->set_table_for_first_display
            EXPORTING
              is_layout                     = lwa_layout
            CHANGING
              it_outtab                     = git_saida_cat02[]
              it_fieldcatalog               = git_fcat_cat02
            EXCEPTIONS
              invalid_parameter_combination = 1
              program_error                 = 2
              too_many_lines                = 3
              OTHERS                        = 4.
        ENDIF.

      ELSE.
        IF gva_cat = '01'.

          CALL METHOD gob_gui_alv_grid->set_table_for_first_display
            EXPORTING
              is_layout                     = lwa_layout
            CHANGING
              it_outtab                     = git_saida_cat01_m[]
              it_fieldcatalog               = git_fcat_cat01_m
            EXCEPTIONS
              invalid_parameter_combination = 1
              program_error                 = 2
              too_many_lines                = 3
              OTHERS                        = 4.
        ELSE.


          CALL METHOD gob_gui_alv_grid->set_table_for_first_display
            EXPORTING
              is_layout                     = lwa_layout
            CHANGING
              it_outtab                     = git_saida_cat02_m[]
              it_fieldcatalog               = git_fcat_cat02_m
            EXCEPTIONS
              invalid_parameter_combination = 1
              program_error                 = 2
              too_many_lines                = 3
              OTHERS                        = 4.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.
  CALL METHOD gob_gui_alv_grid->check_changed_data.
  CALL METHOD gob_gui_alv_grid->refresh_table_display
    EXPORTING
      is_stable = ls_stable
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_cria_fieldcat.
  TYPES: lit_fieldcat_aux TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.

  IF p_linst EQ 'X'.
    CASE p_tipo.
      WHEN '0001'.
        git_local_inst_001 = VALUE lit_fieldcat_aux(
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'GRUPO'      coltext = 'Grupo'                                                     col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'EMPRESA'    coltext = 'Empresa'                                                   col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'CENTRO'     coltext = 'Centro'                                                    col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'SETOR'      coltext = 'Setor'                                                     col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'EQSUP'      coltext = 'Equipamento Superior'                                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'EQINF'      coltext = 'Equipamento Inferior'                                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'FUNCLOC'    coltext = 'Identificação do local de instalação'                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'STRIND'     coltext = 'Código da estrutura do local de instalação'                col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'CATEGORY'   coltext = 'Categoria do local de instalação'                          col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'PLTXT'      coltext = 'Denominação do local de instalação'                        col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'AUTHGRP'    coltext = 'Grupo de autorizações '                                    col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'OBJ_WEIGHT' coltext = 'Peso do objeto'                                            col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'UNIT_OF_WT' coltext = 'Unidade de peso'                                           col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'OBJ_SIZE'   coltext = 'Tamanho/dimensão'                                          col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'INVENTORY'  coltext = 'Número inventário'                                         col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'ACQDATE'    coltext = 'Data da entrada em serviço'                                col_opt = 'X' no_zero = ''  checktable = 'X' datatype = 'DATS' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'OBJECTTYPE' coltext = 'Tipo do objeto técnico'                                    col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'ACQUISVAL'  coltext = 'Valor de aquisição'                                        col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'CURRENCY'   coltext = 'Código da moeda'                                           col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'ANSDT'      coltext = 'Data de aquisição'                                         col_opt = 'X' no_zero = ''  checktable = 'X' datatype = 'DATS' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'MANFACTURE' coltext = 'Fabricante do imobilizado'                                 col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'MANCOUNTRY' coltext = 'País produtor'                                             col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'MANMODEL'   coltext = 'Denominação do tipo atribuído'                             col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'CONSTYEAR'  coltext = 'Ano de construção'                                         col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'CONSTMONTH' coltext = 'Mês da construção'                                         col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'MANPARNO'   coltext = 'Número de vida'                                            col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'MANSERNO'   coltext = 'Número de série de acordo com fabricant'                   col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'MAINTPLANT' coltext = 'Centro de manutenção'                                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'MAINTLOC'   coltext = 'Localização do objeto de manutenção'                       col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'MAINTROOM'  coltext = 'Sala'                                                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'PLSECTN'    coltext = 'Area operacional'                                          col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'WORK_CTR'   coltext = 'Centro de trabalho1'                                       col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'ABCINDIC'   coltext = 'Código ABC para o objeto técnico'                          col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'SORTFIELD'  coltext = 'Campo de ordenaçãoo'                                       col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'COMP_CODE'  coltext = 'Empresa'                                                   col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'BUS_AREA'   coltext = 'Divisão'                                                   col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'ASSET_NO'   coltext = 'Número principal do imobilizado'                           col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'SUB_NUMBER' coltext = 'Sub número do imobilizado'                                 col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'COSTCENTER' coltext = 'Centro de custo'                                           col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'WBS_ELEM'   coltext = 'Elemento do plano da estrutura'                            col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'STANDORDER' coltext = 'Número ordem permanente'                                   col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'SETTLORDER' coltext = 'Ordem p/apropriação de custos'                             col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'PLANPLANT'  coltext = 'Centro de planejamento de manutenção'                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'PLANGROUP'  coltext = 'Grupo de planejamento'                                     col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'GEWRK'      coltext = 'Centro de trabalho responsável'                            col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'WERGW'      coltext = 'Centro relativo ao centro de trabalho'                     col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'CATPROFILE' coltext = 'Perfil do catálogo'                                        col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'EQINSTALL'  coltext = 'Montagem de equipamentos'                                  col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'SUPFLOC'    coltext = 'Local de instalação superior'                              col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'CONSTTYPE'  coltext = 'Material do conjunto do objeto'                            col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'POSNR'      coltext = 'Item no local de instalação superior'                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'EQSINGLE'   coltext = 'Montagem individual de equipamento'                        col_opt = 'X' no_zero = ''  checktable = 'X' )
         ).
      WHEN '0002'.
        git_local_inst_002 = VALUE lit_fieldcat_aux(
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'GRUPO'      coltext = 'Grupo'                                                     col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'EMPRESA'    coltext = 'Empresa'                                                   col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'CENTRO'     coltext = 'Centro'                                                    col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'SETOR'      coltext = 'Setor'                                                     col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'EQSUP'      coltext = 'Equipamento Superior'                                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'EQINF'      coltext = 'Equipamento Inferior'                                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'FUNCLOC'    coltext = 'Identificação do local de instalação'                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'STRIND'     coltext = 'Código da estrutura do local de instalação'                col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'CATEGORY'   coltext = 'Categoria do local de instalação'                          col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'PLTXT'      coltext = 'Denominação do local de instalação'                        col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'AUTHGRP'    coltext = 'Grupo de autorizações '                                    col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'OBJ_WEIGHT' coltext = 'Peso do objeto'                                            col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'UNIT_OF_WT' coltext = 'Unidade de peso'                                           col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'OBJ_SIZE'   coltext = 'Tamanho/dimensão'                                          col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'INVENTORY'  coltext = 'Número inventário'                                         col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'ACQDATE'    coltext = 'Data da entrada em serviço'                                col_opt = 'X' no_zero = ''  checktable = 'X' datatype = 'DATS' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'OBJECTTYPE' coltext = 'Tipo do objeto técnico'                                    col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'ACQUISVAL'  coltext = 'Valor de aquisição'                                        col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'CURRENCY'   coltext = 'Código da moeda'                                           col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'ANSDT'      coltext = 'Data de aquisição'                                         col_opt = 'X' no_zero = ''  checktable = 'X' datatype = 'DATS' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'MANFACTURE' coltext = 'Fabricante do imobilizado'                                 col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'MANCOUNTRY' coltext = 'País produtor'                                             col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'MANMODEL'   coltext = 'Denominação do tipo atribuído'                             col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'CONSTYEAR'  coltext = 'Ano de construção'                                         col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'CONSTMONTH' coltext = 'Mês da construção'                                         col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'MANPARNO'   coltext = 'Número de vida'                                            col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'MANSERNO'   coltext = 'Número de série de acordo com fabricant'                   col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'MAINTPLANT' coltext = 'Centro de manutenção'                                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'MAINTLOC'   coltext = 'Localização do objeto de manutenção'                       col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'MAINTROOM'  coltext = 'Sala'                                                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'PLSECTN'    coltext = 'Area operacional'                                          col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'WORK_CTR'   coltext = 'Centro de trabalho1'                                       col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'ABCINDIC'   coltext = 'Código ABC para o objeto técnico'                          col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'SORTFIELD'  coltext = 'Campo de ordenaçãoo'                                       col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'COMP_CODE'  coltext = 'Empresa'                                                   col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'BUS_AREA'   coltext = 'Divisão'                                                   col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'ASSET_NO'   coltext = 'Número principal do imobilizado'                           col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'SUB_NUMBER' coltext = 'Sub número do imobilizado'                                 col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'COSTCENTER' coltext = 'Centro de custo'                                           col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'WBS_ELEM'   coltext = 'Elemento do plano da estrutura'                            col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'STANDORDER' coltext = 'Número ordem permanente'                                   col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'SETTLORDER' coltext = 'Ordem p/apropriação de custos'                             col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'PLANPLANT'  coltext = 'Centro de planejamento de manutenção'                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'PLANGROUP'  coltext = 'Grupo de planejamento'                                     col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'GEWRK'      coltext = 'Centro de trabalho responsável'                            col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'WERGW'      coltext = 'Centro relativo ao centro de trabalho'                     col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'CATPROFILE' coltext = 'Perfil do catálogo'                                        col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'EQINSTALL'  coltext = 'Montagem de equipamentos'                                  col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'SUPFLOC'    coltext = 'Local de instalação superior'                              col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'CONSTTYPE'  coltext = 'Material do conjunto do objeto'                            col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'POSNR'      coltext = 'Item no local de instalação superior'                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'EQSINGLE'   coltext = 'Montagem individual de equipamento'                        col_opt = 'X' no_zero = ''  checktable = 'X' )
         ).
      WHEN OTHERS.
    ENDCASE.
    RETURN.
  ELSEIF p_linstm EQ 'X'.
    CASE p_tipo.
      WHEN '0001'.
        git_local_inst_001 = VALUE lit_fieldcat_aux(
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'GRUPO'      coltext = 'Grupo'                                                     col_opt = 'X' no_zero = ''  checktable = 'X' no_out = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'EMPRESA'    coltext = 'Empresa'                                                   col_opt = 'X' no_zero = ''  checktable = 'X' no_out = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'CENTRO'     coltext = 'Centro'                                                    col_opt = 'X' no_zero = ''  checktable = 'X' no_out = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'SETOR'      coltext = 'Setor'                                                     col_opt = 'X' no_zero = ''  checktable = 'X' no_out = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'EQSUP'      coltext = 'Equipamento Superior'                                      col_opt = 'X' no_zero = ''  checktable = 'X' no_out = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'EQINF'      coltext = 'Equipamento Inferior'                                      col_opt = 'X' no_zero = ''  checktable = 'X' no_out = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'FUNCLOC'    coltext = 'Identificação do local de instalação'                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'STRIND'     coltext = 'Código da estrutura do local de instalação'                col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'CATEGORY'   coltext = 'Categoria do local de instalação'                          col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'PLTXT'      coltext = 'Denominação do local de instalação'                        col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'AUTHGRP'    coltext = 'Grupo de autorizações '                                    col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'OBJ_WEIGHT' coltext = 'Peso do objeto'                                            col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'UNIT_OF_WT' coltext = 'Unidade de peso'                                           col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'OBJ_SIZE'   coltext = 'Tamanho/dimensão'                                          col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'INVENTORY'  coltext = 'Número inventário'                                         col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'ACQDATE'    coltext = 'Data da entrada em serviço'                                col_opt = 'X' no_zero = ''  checktable = 'X' datatype = 'DATS' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'OBJECTTYPE' coltext = 'Tipo do objeto técnico'                                    col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'ACQUISVAL'  coltext = 'Valor de aquisição'                                        col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'CURRENCY'   coltext = 'Código da moeda'                                           col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'ANSDT'      coltext = 'Data de aquisição'                                         col_opt = 'X' no_zero = ''  checktable = 'X' datatype = 'DATS' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'MANFACTURE' coltext = 'Fabricante do imobilizado'                                 col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'MANCOUNTRY' coltext = 'País produtor'                                             col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'MANMODEL'   coltext = 'Denominação do tipo atribuído'                             col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'CONSTYEAR'  coltext = 'Ano de construção'                                         col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'CONSTMONTH' coltext = 'Mês da construção'                                         col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'MANPARNO'   coltext = 'Número de vida'                                            col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'MANSERNO'   coltext = 'Número de série de acordo com fabricant'                   col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'MAINTPLANT' coltext = 'Centro de manutenção'                                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'MAINTLOC'   coltext = 'Localização do objeto de manutenção'                       col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'MAINTROOM'  coltext = 'Sala'                                                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'PLSECTN'    coltext = 'Area operacional'                                          col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'WORK_CTR'   coltext = 'Centro de trabalho1'                                       col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'ABCINDIC'   coltext = 'Código ABC para o objeto técnico'                          col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'SORTFIELD'  coltext = 'Campo de ordenaçãoo'                                       col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'COMP_CODE'  coltext = 'Empresa'                                                   col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'BUS_AREA'   coltext = 'Divisão'                                                   col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'ASSET_NO'   coltext = 'Número principal do imobilizado'                           col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'SUB_NUMBER' coltext = 'Sub número do imobilizado'                                 col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'COSTCENTER' coltext = 'Centro de custo'                                           col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'WBS_ELEM'   coltext = 'Elemento do plano da estrutura'                            col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'STANDORDER' coltext = 'Número ordem permanente'                                   col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'SETTLORDER' coltext = 'Ordem p/apropriação de custos'                             col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'PLANPLANT'  coltext = 'Centro de planejamento de manutenção'                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'PLANGROUP'  coltext = 'Grupo de planejamento'                                     col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'GEWRK'      coltext = 'Centro de trabalho responsável'                            col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'WERGW'      coltext = 'Centro relativo ao centro de trabalho'                     col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'CATPROFILE' coltext = 'Perfil do catálogo'                                        col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'EQINSTALL'  coltext = 'Montagem de equipamentos'                                  col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'SUPFLOC'    coltext = 'Local de instalação superior'                              col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'CONSTTYPE'  coltext = 'Material do conjunto do objeto'                            col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'POSNR'      coltext = 'Item no local de instalação superior'                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_001'  fieldname = 'EQSINGLE'   coltext = 'Montagem individual de equipamento'                        col_opt = 'X' no_zero = ''  checktable = 'X' )
         ).
      WHEN '0002'.
        git_local_inst_002 = VALUE lit_fieldcat_aux(
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'GRUPO'      coltext = 'Grupo'                                                     col_opt = 'X' no_zero = ''  checktable = 'X' no_out = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'EMPRESA'    coltext = 'Empresa'                                                   col_opt = 'X' no_zero = ''  checktable = 'X' no_out = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'CENTRO'     coltext = 'Centro'                                                    col_opt = 'X' no_zero = ''  checktable = 'X' no_out = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'SETOR'      coltext = 'Setor'                                                     col_opt = 'X' no_zero = ''  checktable = 'X' no_out = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'EQSUP'      coltext = 'Equipamento Superior'                                      col_opt = 'X' no_zero = ''  checktable = 'X' no_out = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'EQINF'      coltext = 'Equipamento Inferior'                                      col_opt = 'X' no_zero = ''  checktable = 'X' no_out = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'FUNCLOC'    coltext = 'Identificação do local de instalação'                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'STRIND'     coltext = 'Código da estrutura do local de instalação'                col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'CATEGORY'   coltext = 'Categoria do local de instalação'                          col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'PLTXT'      coltext = 'Denominação do local de instalação'                        col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'AUTHGRP'    coltext = 'Grupo de autorizações '                                    col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'OBJ_WEIGHT' coltext = 'Peso do objeto'                                            col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'UNIT_OF_WT' coltext = 'Unidade de peso'                                           col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'OBJ_SIZE'   coltext = 'Tamanho/dimensão'                                          col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'INVENTORY'  coltext = 'Número inventário'                                         col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'ACQDATE'    coltext = 'Data da entrada em serviço'                                col_opt = 'X' no_zero = ''  checktable = 'X' datatype = 'DATS' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'OBJECTTYPE' coltext = 'Tipo do objeto técnico'                                    col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'ACQUISVAL'  coltext = 'Valor de aquisição'                                        col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'CURRENCY'   coltext = 'Código da moeda'                                           col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'ANSDT'      coltext = 'Data de aquisição'                                         col_opt = 'X' no_zero = ''  checktable = 'X' datatype = 'DATS' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'MANFACTURE' coltext = 'Fabricante do imobilizado'                                 col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'MANCOUNTRY' coltext = 'País produtor'                                             col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'MANMODEL'   coltext = 'Denominação do tipo atribuído'                             col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'CONSTYEAR'  coltext = 'Ano de construção'                                         col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'CONSTMONTH' coltext = 'Mês da construção'                                         col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'MANPARNO'   coltext = 'Número de vida'                                            col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'MANSERNO'   coltext = 'Número de série de acordo com fabricant'                   col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'MAINTPLANT' coltext = 'Centro de manutenção'                                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'MAINTLOC'   coltext = 'Localização do objeto de manutenção'                       col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'MAINTROOM'  coltext = 'Sala'                                                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'PLSECTN'    coltext = 'Area operacional'                                          col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'WORK_CTR'   coltext = 'Centro de trabalho1'                                       col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'ABCINDIC'   coltext = 'Código ABC para o objeto técnico'                          col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'SORTFIELD'  coltext = 'Campo de ordenaçãoo'                                       col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'COMP_CODE'  coltext = 'Empresa'                                                   col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'BUS_AREA'   coltext = 'Divisão'                                                   col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'ASSET_NO'   coltext = 'Número principal do imobilizado'                           col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'SUB_NUMBER' coltext = 'Sub número do imobilizado'                                 col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'COSTCENTER' coltext = 'Centro de custo'                                           col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'WBS_ELEM'   coltext = 'Elemento do plano da estrutura'                            col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'STANDORDER' coltext = 'Número ordem permanente'                                   col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'SETTLORDER' coltext = 'Ordem p/apropriação de custos'                             col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'PLANPLANT'  coltext = 'Centro de planejamento de manutenção'                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'PLANGROUP'  coltext = 'Grupo de planejamento'                                     col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'GEWRK'      coltext = 'Centro de trabalho responsável'                            col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'WERGW'      coltext = 'Centro relativo ao centro de trabalho'                     col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'CATPROFILE' coltext = 'Perfil do catálogo'                                        col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'EQINSTALL'  coltext = 'Montagem de equipamentos'                                  col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'SUPFLOC'    coltext = 'Local de instalação superior'                              col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'CONSTTYPE'  coltext = 'Material do conjunto do objeto'                            col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'POSNR'      coltext = 'Item no local de instalação superior'                      col_opt = 'X' no_zero = ''  checktable = 'X' )
        ( tabname = 'GIT_SAIDA_LOCAL_INST_002'  fieldname = 'EQSINGLE'   coltext = 'Montagem individual de equipamento'                        col_opt = 'X' no_zero = ''  checktable = 'X' )
         ).
      WHEN OTHERS.
    ENDCASE.
    RETURN.
  ENDIF.

*** CRIAR EQUIPAMENTO
  IF  p_equipc = 'X'.
    IF gva_cat = '01'.

      git_fcat_cat01 = VALUE lit_fieldcat_aux(
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'STATUS'           coltext = 'Status'                                                 col_opt = 'X' no_zero = ''  checktable = 'X'  )
      ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'EXTERNAL_NUMBER'  coltext = 'Nº equipamento'                                          col_opt = 'X' no_zero = ''  checktable = 'X'  )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'VALID_DATE'     coltext = 'Data de validade'                                          col_opt = 'X' no_zero = ''  checktable = 'X' datatype     = 'DATS' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'EQUICATGRY'     coltext = 'Categoria de equipamento'                                  col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'AUTHGRP'        coltext = 'Grupo de autorizações referente ao objeto técnico'         col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'OBJ_WEIGHT'     coltext = 'Peso do objeto'                                            col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'UNIT_OF_WT'     coltext = 'Unidade de peso'                                           col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'OBJ_SIZE'       coltext = 'Tamanho/dimensão'                                          col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'INVENTORY'      coltext = 'Nº inventário'                                             col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'START_FROM'     coltext = 'Data da entrada em serviço do objeto técnico'              col_opt = 'X' no_zero = ''  checktable = 'X' datatype     = 'DATS' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'OBJECTTYPE'     coltext = 'Tipo do objeto técnico'                                    col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'ACQUISVAL'      coltext = 'Valor de aquisição'                                        col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'CURRENCY'       coltext = 'Código da moeda'                                           col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'ACQDATE'        coltext = 'Data de aquisição'                                         col_opt = 'X' no_zero = ''  checktable = 'X' datatype     = 'DATS' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'MANFACTURE'     coltext = 'Fabricante do imobilizado'                                 col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'MANCOUNTRY'     coltext = 'País produtor'                                             col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'MANMODEL'       coltext = 'Denominação do tipo atribuído pelo fabricante'             col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'CONSTYEAR'      coltext = 'Ano de construção'                                         col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'CONSTMONTH'     coltext = 'Mês da construção'                                         col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'MANPARNO'       coltext = 'Nº de Vida'                                                col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'MANSERNO'       coltext = 'Nº de série de acordo com o fabricante'                    col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'DESCRIPT'       coltext = 'Denominação do objeto técnico'                             col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'DATAB'          coltext = 'Data início validade'                                      col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'MAINTPLANT'     coltext = 'Centro de manutenção'                                      col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'MAINTLOC'       coltext = 'Localização do objeto de manutenção'                       col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'MAINTROOM'      coltext = 'Sala'                                                      col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'PLSECTN'        coltext = 'Área operacional'                                          col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'PP_WKCTR'       coltext = 'Centro de trabalho'                                        col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'ABCINDIC'       coltext = 'Código ABC para o objeto técnico'                          col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'SORTFIELD'      coltext = 'Campo de ordenação'                                        col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'COMP_CODE'      coltext = 'Empresa'                                                   col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'BUS_AREA'       coltext = 'Divisão'                                                   col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'ASSET_NO'       coltext = 'Nº principal do imobilizado'                               col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'PLANPLANT'      coltext = 'Centro de planejamento de manutenção'                      col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'SUB_NUMBER'     coltext = 'Subnº do imobilizado'                                      col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'COSTCENTER'     coltext = 'Centro de custo'                                           col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'WBS_ELEM'       coltext = 'Elemento do plano da estrutura do projeto (elemento PEP)'  col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'STANDORDER'     coltext = 'Nº ordem permanente'                                       col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'SETTLORDER'     coltext = 'Ordem p/apropriação de custos'                             col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'PLANGROUP'      coltext = 'Grupo de planejamento para serviços cliente e manutenção'  col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'WORK_CTR'       coltext = 'Centro de trabalho responsável para medidas de manutenção' col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'CATPROFILE'     coltext = 'Perfil do catálogo'                                        col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'WERGW'          coltext = 'Centro relativo ao centro de trabalho responsável'         col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'DATUM'          coltext = 'Data na qual o loc.montagem do equipamento foi modificado' col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'UZEIT'          coltext = 'Hora na qual o loc.montagem do equipamento foi modificado' col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'READ_FLOC'      coltext = 'Local de instalação'                                       col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'CONSTTYPE'      coltext = 'Material do conjunto do objeto técnico'                    col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'INST_POS'       coltext = 'Item no local de instalação superior'                      col_opt = 'X' no_zero = ''  checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01'  fieldname = 'TECHID'         coltext = 'Nº identificação técnica'                                  col_opt = 'X' no_zero = ''  checktable = 'X' )
       ).
    ELSE.
*** FROTA
      IF gva_cat = '02'.
        git_fcat_cat02 = VALUE lit_fieldcat_aux(
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'STATUS'                   coltext = 'Status'                                                               col_opt = 'X' no_zero = ''  checktable = 'X'  )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'EXTERNAL_NUMBER'          coltext = 'Nº equipamento'                                                       col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'VALID_DATE'               coltext = 'Data de validade'                                                     col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' datatype     = 'DATS' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'EQUICATGRY'               coltext = 'Categoria de equipamento'                                             col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'AUTHGRP'                  coltext = 'Grupo de autorizações referente ao objeto técnico'                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'OBJ_WEIGHT'               coltext = 'Peso do objeto'                                                       col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'UNIT_OF_WT'               coltext = 'Unidade de peso'                                                      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'OBJ_SIZE'                 coltext = 'Tamanho/dimensão'                                                     col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'INVENTORY'                coltext = 'Nº inventário'                                                        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'START_FROM'               coltext = 'Data da entrada em serviço do objeto técnico'                         col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' datatype     = 'DATS' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'OBJECTTYPE'               coltext = 'Tipo do objeto técnico'                                               col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'ACQUISVAL'                coltext = 'Valor de aquisição'                                                   col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'CURRENCY'                 coltext = 'Código da moeda'                                                      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'ACQDATE'                  coltext = 'Data de aquisição'                                                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' datatype     = 'DATS' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'MANFACTURE'               coltext = 'Fabricante do imobilizado'                                            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'MANCOUNTRY'               coltext = 'País produtor'                                                        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'MANMODEL'                 coltext = 'Denominação do tipo atribuído pelo fabricante'                        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'CONSTYEAR'                coltext = 'Ano de construção'                                                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'CONSTMONTH'               coltext = 'Mês da construção'                                                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'MANPARNO'                 coltext = 'Nº de Vida'                                                           col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'MANSERNO'                 coltext = 'Nº de série de acordo com o fabricante'                               col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'DESCRIPT'                 coltext = 'Denominação do objeto técnico'                                        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'DATAB'                    coltext = 'Data início validade'                                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' datatype     = 'DATS' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'MAINTPLANT'               coltext = 'Centro de manutenção'                                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'MAINTLOC'                 coltext = 'Localização do objeto de manutenção'                                  col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'MAINTROOM'                coltext = 'Sala'                                                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'PLSECTN'                  coltext = 'Área operacional'                                                     col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'PP_WKCTR'                 coltext = 'Centro de trabalho'                                                   col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'ABCINDIC'                 coltext = 'Código ABC para o objeto técnico'                                     col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'SORTFIELD'                coltext = 'Campo de ordenação'                                                   col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'COMP_CODE'                coltext = 'Empresa'                                                              col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'BUS_AREA'                 coltext = 'Divisão'                                                              col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'ASSET_NO'                 coltext = 'Nº principal do imobilizado'                                          col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'COSTCENTER'               coltext = 'Centro de custo'                                                      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'WBS_ELEM'                 coltext = 'Elemento do plano da estrutura do projeto (elemento PEP)'             col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'STANDORDER'               coltext = 'Nº ordem permanente'                                                  col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'SETTLORDER'               coltext = 'Ordem p/apropriação de custos'                                        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'PLANPLANT'                coltext = 'Centro de planejamento de manutenção'                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'PLANGROUP'                coltext = 'Grupo de planejamento para serviços cliente e manutenção'             col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'WORK_CTR'                 coltext = 'Centro de trabalho responsável para medidas de manutenção'            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'CATPROFILE'               coltext = 'Perfil do catálogo'                                                   col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'READ_FLOC'                coltext = 'Local de instalação'                                                  col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'DATUM'                    coltext = 'Data na qual o loc.montagem do equipamento foi modificado'            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' datatype     = 'DATS' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'UZEIT'                    coltext = 'Hora na qual o loc.montagem do equipamento foi modificado'            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'CONSTTYPE'                coltext = 'Material do conjunto do objeto técnico'                               col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'INST_POS'                 coltext = 'Item no local de instalação superior'                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'TECHID'                   coltext = 'Nº identificação técnica'                                             col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'FLEET_NUM'                coltext = 'Nº identificação para o veículo'                                      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'LICENSE_NUM'              coltext = 'Placa de veículo'                                                     col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'EXPIRY_DATE'              coltext = 'Fim da validade'                                                      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'FLEET_VIN'                coltext = 'Nºidentificação do fabricante para o veículo'                         col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'CHASSIS_NUM'              coltext = 'Número do chassis'                                                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'GROSS_WGT'                coltext = 'Peso total permitido'                                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'LOAD_WGT'                 coltext = 'Carga máxima (peso)'                                                  col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'LOAD_VOL'                 coltext = 'Volume plat.carga'                                                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'VOL_UNIT'                 coltext = 'Unidade de volume'                                                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'LOAD_HGT'                 coltext = 'Altura do espaço de carga'                                            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'LOAD_DIM_UNIT'            coltext = 'Unidade comprimento p/o espaço de carga'                              col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'LOAD_WID'                 coltext = 'Largura plat.carga'                                                   col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'LOAD_LEN'                 coltext = 'Compr.plat.carga'                                                     col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'NO_COMPART'               coltext = 'Número de compartimentos'                                             col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'FLEET_HGT'                coltext = 'Altura do veículo'                                                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'DIM_UNIT'                 coltext = 'Unid.comprimento'                                                     col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'FLEET_WID'                coltext = 'Largura do veículo'                                                   col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'FLEET_LEN'                coltext = 'Comprimento do veículo'                                               col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'REPLA_DATE'               coltext = 'Data, na qual o veículo deverá ser substituído'                       col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' datatype     = 'DATS' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'REPLA_ODOM'               coltext = 'Posição hodômetro, na qual o objeto deverá ser substituído'           col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'REPLA_OPH'                coltext = 'Pos.contador tempo na qual o veículo deve ser substituído'            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'FLEET_USE'                coltext = 'Código de utilização'                                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'CARD_NUM'                 coltext = 'Nºcartão combustível'                                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'MAX_OCCUPANTS'            coltext = 'Nºmáximo permitido de passageiros'                                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'KEY_NUM'                  coltext = 'Tq Comb'                                                              col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'NUM_AXLE'                 coltext = 'Número de eixos do veículo'                                           col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'ENGINE_TYPE'              coltext = 'Tipo de tração'                                                       col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'ENGINE_SNR'               coltext = 'Nºsérie motor do fabricante'                                          col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'SPEED_MAX'                coltext = 'Velocidade máxima do veículo'                                         col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'SPEED_UNIT'               coltext = 'Unidade para a velocidade'                                            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'ENGINE_POWER'             coltext = 'Desempenho em relação ao núm.indicado de rotações'                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'UNIT_POWER'               coltext = 'Unidade de medida para a potência'                                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'REVOLUTIONS'              coltext = 'Número de rotações por minuto'                                        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'ENGINE_CAP'               coltext = 'Cilindrada'                                                           col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'UNIT_CAP'                 coltext = 'Unidade para a cilindrada'                                            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'ENGINE_CYL'               coltext = 'Núm.de cilindros'                                                     col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'FUEL_PRI'                 coltext = 'Tipo carburante do combustível primário'                              col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'FUEL_SEC'                 coltext = 'Tipo carburante do combustível secundário'                            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'OIL_TYPE'                 coltext = 'Tipo carburante óleo'                                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'PRI_CALC'                 coltext = 'Chave do método de cálculo para vlrs.consumo'                         col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'DIV1'                     coltext = 'Dispositivo Identificador do Veículo'                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'TQ_COMBUSTIVEL_1'         coltext = 'Capacidade do Tanque de combustível'                                  col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'DIV2'                     coltext = 'Dispositivo Identificador do Veículo'                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'TQ_COMBUSTIVEL_2'         coltext = 'Capacidade do Tanque de combustível'                                  col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'DIV3'                     coltext = 'Dispositivo Identificador do Veículo'                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
  ( tabname = 'GIT_SAIDA_CAT02'  fieldname = 'TQ_COMBUSTIVEL_3'         coltext = 'Capacidade do Tanque de combustível'                                  col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
   ).
      ENDIF.
    ENDIF.
  ELSE.
*** Modificar Equipamento
    IF  p_equipm = 'X'.
      IF gva_cat = '01'.

        git_fcat_cat01_m = VALUE lit_fieldcat_aux(
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'STATUS'         coltext = 'Status'                                                    col_opt = 'X' no_zero = ''  checktable = 'X'  )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'EQUIPMENT'      coltext = 'Nº equipamento'                                            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'VALID_DATE'     coltext = 'Data de validade'                                          col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' datatype     = 'DATS' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'VALID_DATE'     coltext = 'Data de validade'                                          col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' datatype     = 'DATS' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'EQUICATGRY'     coltext = 'Categoria de equipamento'                                  col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'AUTHGRP'        coltext = 'Grupo de autorizações referente ao objeto técnico'         col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'OBJ_WEIGHT'     coltext = 'Peso do objeto'                                            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'UNIT_OF_WT'     coltext = 'Unidade de peso'                                           col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'OBJ_SIZE'       coltext = 'Tamanho/dimensão'                                          col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'INVENTORY'      coltext = 'Nº inventário'                                             col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'START_FROM'     coltext = 'Data da entrada em serviço do objeto técnico'              col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' datatype     = 'DATS' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'OBJECTTYPE'     coltext = 'Tipo do objeto técnico'                                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'ACQUISVAL'      coltext = 'Valor de aquisição'                                        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'CURRENCY'       coltext = 'Código da moeda'                                           col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'ACQDATE'        coltext = 'Data de aquisição'                                         col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' datatype     = 'DATS' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'MANFACTURE'     coltext = 'Fabricante do imobilizado'                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'MANCOUNTRY'     coltext = 'País produtor'                                             col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'MANMODEL'       coltext = 'Denominação do tipo atribuído pelo fabricante'             col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'CONSTYEAR'      coltext = 'Ano de construção'                                         col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'CONSTMONTH'     coltext = 'Mês da construção'                                         col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'MANPARNO'       coltext = 'Nº de Vida'                                                col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'MANSERNO'       coltext = 'Nº de série de acordo com o fabricante'                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'DESCRIPT'       coltext = 'Denominação do objeto técnico'                             col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'DATAB'          coltext = 'Data início validade'                                      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'MAINTPLANT'     coltext = 'Centro de manutenção'                                      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'MAINTLOC'       coltext = 'Localização do objeto de manutenção'                       col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'MAINTROOM'      coltext = 'Sala'                                                      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'PLSECTN'        coltext = 'Área operacional'                                          col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'PP_WKCTR'       coltext = 'Centro de trabalho'                                        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'ABCINDIC'       coltext = 'Código ABC para o objeto técnico'                          col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'SORTFIELD'      coltext = 'Campo de ordenação'                                        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'COMP_CODE'      coltext = 'Empresa'                                                   col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'BUS_AREA'       coltext = 'Divisão'                                                   col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'ASSET_NO'       coltext = 'Nº principal do imobilizado'                               col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'PLANPLANT'      coltext = 'Centro de planejamento de manutenção'                      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'SUB_NUMBER'     coltext = 'Subnº do imobilizado'                                      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'COSTCENTER'     coltext = 'Centro de custo'                                           col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'WBS_ELEM'       coltext = 'Elemento do plano da estrutura do projeto (elemento PEP)'  col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'STANDORDER'     coltext = 'Nº ordem permanente'                                       col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'SETTLORDER'     coltext = 'Ordem p/apropriação de custos'                             col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'PLANGROUP'      coltext = 'Grupo de planejamento para serviços cliente e manutenção'  col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'WORK_CTR'       coltext = 'Centro de trabalho responsável para medidas de manutenção' col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'CATPROFILE'     coltext = 'Perfil do catálogo'                                        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'WERGW'          coltext = 'Centro relativo ao centro de trabalho responsável'         col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'DATUM'          coltext = 'Data na qual o loc.montagem do equipamento foi modificado' col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'UZEIT'          coltext = 'Hora na qual o loc.montagem do equipamento foi modificado' col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'READ_FLOC'      coltext = 'Local de instalação'                                       col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'CONSTTYPE'      coltext = 'Material do conjunto do objeto técnico'                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'INST_POS'       coltext = 'Item no local de instalação superior'                      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
      ( tabname = 'GIT_SAIDA_CAT01_M'  fieldname = 'TECHID'         coltext = 'Nº identificação técnica'                                  col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
       ).
      ELSE.
*** FROTA
        IF gva_cat = '02'.
          git_fcat_cat02_m = VALUE lit_fieldcat_aux(
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'STATUS'                   coltext = 'Status'                                                                         col_opt = 'X' no_zero = ''  checktable = 'X'  )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'EQUIPMENT'                coltext = 'Nº equipamento'                                                       col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'VALID_DATE'               coltext = 'Data de validade'                                                     col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' datatype     = 'DATS' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'EQUICATGRY'               coltext = 'Categoria de equipamento'                                             col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'AUTHGRP'                  coltext = 'Grupo de autorizações referente ao objeto técnico'                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'OBJ_WEIGHT'               coltext = 'Peso do objeto'                                                       col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'UNIT_OF_WT'               coltext = 'Unidade de peso'                                                      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'OBJ_SIZE'                 coltext = 'Tamanho/dimensão'                                                     col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'INVENTORY'                coltext = 'Nº inventário'                                                        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'START_FROM'               coltext = 'Data da entrada em serviço do objeto técnico'                         col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' datatype     = 'DATS' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'OBJECTTYPE'               coltext = 'Tipo do objeto técnico'                                               col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'ACQUISVAL'                coltext = 'Valor de aquisição'                                                   col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'CURRENCY'                 coltext = 'Código da moeda'                                                      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'ACQDATE'                  coltext = 'Data de aquisição'                                                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' datatype     = 'DATS' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'MANFACTURE'               coltext = 'Fabricante do imobilizado'                                            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'MANCOUNTRY'               coltext = 'País produtor'                                                        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'MANMODEL'                 coltext = 'Denominação do tipo atribuído pelo fabricante'                        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'CONSTYEAR'                coltext = 'Ano de construção'                                                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'CONSTMONTH'               coltext = 'Mês da construção'                                                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'MANPARNO'                 coltext = 'Nº de Vida'                                                           col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'MANSERNO'                 coltext = 'Nº de série de acordo com o fabricante'                               col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'DESCRIPT'                 coltext = 'Denominação do objeto técnico'                                        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'DATAB'                    coltext = 'Data início validade'                                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' datatype     = 'DATS' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'MAINTPLANT'               coltext = 'Centro de manutenção'                                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'MAINTLOC'                 coltext = 'Localização do objeto de manutenção'                                  col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'MAINTROOM'                coltext = 'Sala'                                                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'PLSECTN'                  coltext = 'Área operacional'                                                     col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'PP_WKCTR'                 coltext = 'Centro de trabalho'                                                   col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'ABCINDIC'                 coltext = 'Código ABC para o objeto técnico'                                     col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'SORTFIELD'                coltext = 'Campo de ordenação'                                                   col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'COMP_CODE'                coltext = 'Empresa'                                                              col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'BUS_AREA'                 coltext = 'Divisão'                                                              col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'ASSET_NO'                 coltext = 'Nº principal do imobilizado'                                          col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'COSTCENTER'               coltext = 'Centro de custo'                                                      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'WBS_ELEM'                 coltext = 'Elemento do plano da estrutura do projeto (elemento PEP)'             col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'STANDORDER'               coltext = 'Nº ordem permanente'                                                  col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'SETTLORDER'               coltext = 'Ordem p/apropriação de custos'                                        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'PLANPLANT'                coltext = 'Centro de planejamento de manutenção'                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'PLANGROUP'                coltext = 'Grupo de planejamento para serviços cliente e manutenção'             col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'WORK_CTR'                 coltext = 'Centro de trabalho responsável para medidas de manutenção'            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'CATPROFILE'               coltext = 'Perfil do catálogo'                                                   col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'READ_FLOC'                coltext = 'Local de instalação'                                                  col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'DATUM'                    coltext = 'Data na qual o loc.montagem do equipamento foi modificado'            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' datatype     = 'DATS' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'UZEIT'                    coltext = 'Hora na qual o loc.montagem do equipamento foi modificado'            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'CONSTTYPE'                coltext = 'Material do conjunto do objeto técnico'                               col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'INST_POS'                 coltext = 'Item no local de instalação superior'                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'TECHID'                   coltext = 'Nº identificação técnica'                                             col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'FLEET_NUM'                coltext = 'Nº identificação para o veículo'                                      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'LICENSE_NUM'              coltext = 'Placa de veículo'                                                     col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'EXPIRY_DATE'              coltext = 'Fim da validade'                                                      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'FLEET_VIN'                coltext = 'Nºidentificação do fabricante para o veículo'                         col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'CHASSIS_NUM'              coltext = 'Número do chassis'                                                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'GROSS_WGT'                coltext = 'Peso total permitido'                                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'LOAD_WGT'                 coltext = 'Carga máxima (peso)'                                                  col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'LOAD_VOL'                 coltext = 'Volume plat.carga'                                                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'VOL_UNIT'                 coltext = 'Unidade de volume'                                                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'LOAD_HGT'                 coltext = 'Altura do espaço de carga'                                            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'LOAD_DIM_UNIT'            coltext = 'Unidade comprimento p/o espaço de carga'                              col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'LOAD_WID'                 coltext = 'Largura plat.carga'                                                   col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'LOAD_LEN'                 coltext = 'Compr.plat.carga'                                                     col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'NO_COMPART'               coltext = 'Número de compartimentos'                                             col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'FLEET_HGT'                coltext = 'Altura do veículo'                                                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'DIM_UNIT'                 coltext = 'Unid.comprimento'                                                     col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'FLEET_WID'                coltext = 'Largura do veículo'                                                   col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'FLEET_LEN'                coltext = 'Comprimento do veículo'                                               col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'REPLA_DATE'               coltext = 'Data, na qual o veículo deverá ser substituído'                       col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' datatype     = 'DATS' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'REPLA_ODOM'               coltext = 'Posição hodômetro, na qual o objeto deverá ser substituído'           col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'REPLA_OPH'                coltext = 'Pos.contador tempo na qual o veículo deve ser substituído'            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'FLEET_USE'                coltext = 'Código de utilização'                                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'CARD_NUM'                 coltext = 'Nºcartão combustível'                                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'MAX_OCCUPANTS'            coltext = 'Nºmáximo permitido de passageiros'                                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'KEY_NUM'                  coltext = 'Tq Comb'                                                              col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'NUM_AXLE'                 coltext = 'Número de eixos do veículo'                                           col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'ENGINE_TYPE'              coltext = 'Tipo de tração'                                                       col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'ENGINE_SNR'               coltext = 'Nºsérie motor do fabricante'                                          col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'SPEED_MAX'                coltext = 'Velocidade máxima do veículo'                                         col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'SPEED_UNIT'               coltext = 'Unidade para a velocidade'                                            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'ENGINE_POWER'             coltext = 'Desempenho em relação ao núm.indicado de rotações'                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'UNIT_POWER'               coltext = 'Unidade de medida para a potência'                                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'REVOLUTIONS'              coltext = 'Número de rotações por minuto'                                        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'ENGINE_CAP'               coltext = 'Cilindrada'                                                           col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'UNIT_CAP'                 coltext = 'Unidade para a cilindrada'                                            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'ENGINE_CYL'               coltext = 'Núm.de cilindros'                                                     col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'FUEL_PRI'                 coltext = 'Tipo carburante do combustível primário'                              col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'FUEL_SEC'                 coltext = 'Tipo carburante do combustível secundário'                            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'OIL_TYPE'                 coltext = 'Tipo carburante óleo'                                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'PRI_CALC'                 coltext = 'Chave do método de cálculo para vlrs.consumo'                         col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'DIV1'                     coltext = 'Dispositivo Identificador do Veículo'                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'TQ_COMBUSTIVEL_1'         coltext = 'Capacidade do Tanque de combustível'                                  col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'DIV2'                     coltext = 'Dispositivo Identificador do Veículo'                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'TQ_COMBUSTIVEL_2'         coltext = 'Capacidade do Tanque de combustível'                                  col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'DIV3'                     coltext = 'Dispositivo Identificador do Veículo'                                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
    ( tabname = 'GIT_SAIDA_CAT02_M'  fieldname = 'TQ_COMBUSTIVEL_3'         coltext = 'Capacidade do Tanque de combustível'                                  col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' checktable = 'X' )
     ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_FILTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_filtros.

  DATA vl_text TYPE TABLE OF textpool.

  CALL FUNCTION 'RS_TEXTPOOL_READ'
    EXPORTING
      objectname = sy-repid
      action     = 'SHOW'
      language   = sy-langu
    TABLES
      tpool      = vl_text.

  FREE: git_filtro.

*  LOOP AT SCREEN.
*    git_filtro = VALUE #(
*      ( parametro = '' valor = p_bukrs )
*      ( parametro = '' valor = p_werks )
*    ).
*  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_IMPORTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_importa .

  IF p_equipc EQ abap_true. "Criar equipamento.
    CALL  SCREEN 0110 STARTING AT 8 8.
  ELSE."Modificar equipamento.
    FREE: t_status[].
    PERFORM fm_carrega_arquivo.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_PROC_RESULTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_proc_results .


  DATA: lva_ctrlcol1 TYPE alsmex_tabline-value,
        lva_arbpl    TYPE crhd-arbpl,
        lwa_crhd     TYPE crhd.

  DATA: lit_datxls_01     TYPE TABLE OF ty_saida_cat01 WITH HEADER LINE.
  DATA: lit_datxls_02     TYPE TABLE OF ty_saida_cat02 WITH HEADER LINE.
  DATA: lit_datxls_01_m   TYPE TABLE OF ty_saida_cat01_m WITH HEADER LINE.
  DATA: lit_datxls_02_m   TYPE TABLE OF ty_saida_cat02_m WITH HEADER LINE.

  IF p_equipc = 'X'. "Equipamento
    PERFORM fm_set_dados_equipamento. "Seta dados criar equipamento.
  ELSE.
    IF p_equipm = 'X'. "Modificar equipamento
      PERFORM fm_set_dados_mod_equipamento. "Seta dados modificar equipamento.
    ENDIF.
    IF p_linst = 'X'.
      PERFORM fm_set_dados_loc_inst. "Criar local de instalação
    ENDIF.
    IF p_linstm = 'X'.
      PERFORM fm_set_dados_mod_loc_inst. "Modificar local de instalação
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_SAIDA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_saida_tela .
  CALL METHOD gob_gui_alv_grid->refresh_table_display.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_EXECUTAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_executar .

  DATA: lva_equipment LIKE bapi_itob_parms-equipment,
        lva_wait      LIKE bapita-wait,
        lva_return    TYPE bapireturn,
        lva_linha     TYPE sy-tabix,
        lwa_erros     LIKE LINE OF git_erros.

  CLEAR:  git_erros, local.
  FREE: t_status[].


  IF  p_equipc = 'X'.

    IF gva_cat = '01'. "Criar equipamento diversos.
      PERFORM fm_criar_eqpto_div.
    ELSE. "Criar equipamento froa.
      PERFORM fm_criar_eqpto_frota.
    ENDIF.
  ELSE.
    IF p_equipm = 'X'. "Modifica equipamento.
      IF gva_cat = '01'.

        PERFORM fm_fill_modifica.
        PERFORM fm_modifica_eqpto_div. "Modifica equipamento diversos.

      ELSE.
        PERFORM fm_fill_modifica.
        PERFORM fm_modifica_eqpto_frota. "Modifica equipamento frota.
      ENDIF.
    ENDIF.
  ENDIF.

  IF p_linst EQ 'X'.
    PERFORM fm_criar_loc_inst.
  ENDIF.

  IF p_linstm EQ 'X'.
    PERFORM fm_modificar_loc_inst.
  ENDIF.


  "Envia message de processamento.
  MESSAGE s000(z01) WITH 'Dados processado com sucesso, '
                         'acompanhe o resultado no status '
                         'de processamento.'.

  CALL METHOD gob_gui_alv_grid->refresh_table_display.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_VERIFICAR_INFORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_verificar_inform .

  IF p_equipc = 'X'. "Criar equipamento.
    PERFORM fm_check_inform_equipc.
  ELSE.
    IF p_linst IS NOT INITIAL.
      PERFORM fm_check_inform_locinst_criar.
      EXIT.
    ELSEIF  p_linstm IS NOT INITIAL.
      PERFORM fm_check_inform_locinst_mod.
      EXIT.
    ENDIF.
    PERFORM fm_check_inform_equip. "Modificar equipamento.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_LIMPAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM
  fm_limpar_alv .
  CLEAR: git_saida_cat01[].
  CLEAR: git_saida_cat02[].
  CLEAR: git_saida_cat01_m[].
  CLEAR: git_saida_cat02_m[].
  CLEAR: git_saida_local_inst_001[].
  CLEAR: git_saida_local_inst_002[].
  CALL METHOD gob_gui_alv_grid->refresh_table_display.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_ELIM_LINHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_elim_linha .

  DATA: lit_sel_rows TYPE lvc_t_row,
        lwa_sel_rows TYPE lvc_s_row.

  CALL METHOD gob_gui_alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = lit_sel_rows.

  CHECK NOT lit_sel_rows IS INITIAL.
  IF  p_equipc = 'X'.
    IF gva_cat = '01'.
      LOOP AT lit_sel_rows INTO lwa_sel_rows.
        git_saida_cat01-flag = abap_true.
        MODIFY git_saida_cat01 INDEX lwa_sel_rows-index FROM git_saida_cat01 TRANSPORTING flag.
      ENDLOOP.

      DELETE git_saida_cat01 WHERE flag EQ abap_true.
    ELSE.
      LOOP AT lit_sel_rows INTO lwa_sel_rows.
        git_saida_cat02-flag = abap_true.
        MODIFY git_saida_cat02 INDEX lwa_sel_rows-index FROM git_saida_cat02 TRANSPORTING flag.
      ENDLOOP.

      DELETE git_saida_cat02 WHERE flag EQ abap_true.
    ENDIF.
  ELSE.
    IF  p_equipm = 'X'.
      IF gva_cat = '01'.
        LOOP AT lit_sel_rows INTO lwa_sel_rows.
          git_saida_cat01_m-flag = abap_true.
          MODIFY git_saida_cat01_m INDEX lwa_sel_rows-index FROM git_saida_cat01_m TRANSPORTING flag.
        ENDLOOP.

        DELETE git_saida_cat01_m WHERE flag EQ abap_true.
      ELSE.
        LOOP AT lit_sel_rows INTO lwa_sel_rows.
          git_saida_cat02_m-flag = abap_true.
          MODIFY git_saida_cat02_m INDEX lwa_sel_rows-index FROM git_saida_cat02_m TRANSPORTING flag.
        ENDLOOP.

        DELETE git_saida_cat02_m WHERE flag EQ abap_true.
      ENDIF.
    ENDIF.
  ENDIF.

  DATA: ls_saida_local_inst_001 TYPE ty_saida_local_inst_001,
        ls_saida_local_inst_002 TYPE ty_saida_local_inst_002.

  IF p_linst IS NOT INITIAL.
    CASE p_tipo.
      WHEN '0001'.
        LOOP AT lit_sel_rows INTO lwa_sel_rows.
          MODIFY git_saida_local_inst_001 INDEX lwa_sel_rows-index FROM ls_saida_local_inst_001.
        ENDLOOP.

        DELETE git_saida_local_inst_001 WHERE empresa IS INITIAL AND grupo IS INITIAL AND setor IS INITIAL AND centro IS INITIAL.
      WHEN '0002'.
        LOOP AT lit_sel_rows INTO lwa_sel_rows.
          MODIFY git_saida_local_inst_002 INDEX lwa_sel_rows-index FROM ls_saida_local_inst_002.
        ENDLOOP.

        DELETE git_saida_local_inst_002 WHERE empresa IS INITIAL AND grupo IS INITIAL AND setor IS INITIAL AND centro IS INITIAL.
      WHEN OTHERS.
    ENDCASE.
  ELSEIF p_linstm IS NOT INITIAL.
    CASE p_tipo.
      WHEN '0001'.
        LOOP AT lit_sel_rows INTO lwa_sel_rows.
          MODIFY git_saida_local_inst_001 INDEX lwa_sel_rows-index FROM ls_saida_local_inst_001.
        ENDLOOP.

        DELETE git_saida_local_inst_001 WHERE funcloc IS INITIAL.
      WHEN '0002'.
        LOOP AT lit_sel_rows INTO lwa_sel_rows.
          MODIFY git_saida_local_inst_002 INDEX lwa_sel_rows-index FROM ls_saida_local_inst_002.
        ENDLOOP.

        DELETE git_saida_local_inst_002 WHERE funcloc IS INITIAL.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.

  CALL METHOD gob_gui_alv_grid->refresh_table_display.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen INPUT.
*  CASE sy-ucomm.
*    WHEN space.
*      IF ( screen-group1 = 'BTN1' ).
*        LOOP AT SCREEN.
*          screen-active = 0.
*          screen-invisible = 1. "to hide button from screen
*        ENDLOOP.
*        MODIFY SCREEN.
*      ENDIF.
*  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_FILL_MODIFICA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_fill_modifica .

  gwa_data_generalx-authgrp       = 'X'.
  gwa_data_generalx-obj_weight    = 'X'.
  gwa_data_generalx-unit_of_wt    = 'X'.
  gwa_data_generalx-obj_size      = 'X'.
  gwa_data_generalx-inventory     = 'X'.
  gwa_data_generalx-start_from    = 'X'.
  gwa_data_generalx-objecttype    = 'X'.
  gwa_data_generalx-acquisval     = 'X'.
  gwa_data_generalx-currency      = 'X'.
  gwa_data_generalx-acqdate       = 'X'.
  gwa_data_generalx-manfacture    = 'X'.
  gwa_data_generalx-mancountry    = 'X'.
  gwa_data_generalx-manmodel      = 'X'.
  gwa_data_generalx-constyear     = 'X'.
  gwa_data_generalx-constmonth    = 'X'.
  gwa_data_generalx-manparno      = 'X'.
  gwa_data_generalx-manserno      = 'X'.
  gwa_data_generalx-descript      = 'X'.
  gwa_data_generalx-start_from    = 'X'.
*  gwa_data_generalx-maintplant    = 'X'.
  gwa_data_generalx-maintloc      = 'X'.
  gwa_data_generalx-maintroom     = 'X'.
  gwa_data_generalx-plsectn       = 'X'.
  gwa_data_generalx-pp_wkctr      = 'X'.
  gwa_data_generalx-abcindic      = 'X'.
  gwa_data_generalx-sortfield     = 'X'.
*  gwa_data_generalx-comp_code     = 'X'.
*  gwa_data_generalx-bus_area      = 'X'.
  gwa_data_generalx-asset_no      = 'X'.
  gwa_data_generalx-costcenter    = 'X'.
  gwa_data_generalx-wbs_elem      = 'X'.
  gwa_data_generalx-standorder    = 'X'.
  gwa_data_generalx-settlorder    = 'X'.
*  gwa_data_generalx-planplant     = 'X'.
  gwa_data_generalx-plangroup     = 'X'.
  gwa_data_generalx-work_ctr      = 'X'.
  gwa_data_generalx-catprofile    = 'X'.
  gwa_data_generalx-consttype     = 'X'.


  gwa_data_specificx-inst_pos     = 'X'.
  gwa_data_specificx-techid       = 'X'.
  gwa_data_specificx-equicatgry   = 'X'.
  gwa_data_specificx-read_floc    = 'X'.

  gwa_data_fleetx-fleet_num       = 'X'.
  gwa_data_fleetx-license_num     = 'X'.
  gwa_data_fleetx-expiry_date     = 'X'.
  gwa_data_fleetx-fleet_vin       = 'X'.
  gwa_data_fleetx-chassis_num     = 'X'.
  gwa_data_fleetx-gross_wgt       = 'X'.
  gwa_data_fleetx-load_wgt        = 'X'.
  gwa_data_fleetx-load_vol        = 'X'.
  gwa_data_fleetx-vol_unit        = 'X'.
  gwa_data_fleetx-load_hgt        = 'X'.
  gwa_data_fleetx-load_dim_unit   = 'X'.
  gwa_data_fleetx-load_wid        = 'X'.
  gwa_data_fleetx-load_len        = 'X'.
  gwa_data_fleetx-no_compart      = 'X'.
  gwa_data_fleetx-fleet_hgt       = 'X'.
  gwa_data_fleetx-dim_unit        = 'X'.
  gwa_data_fleetx-fleet_wid       = 'X'.
  gwa_data_fleetx-fleet_len       = 'X'.
  gwa_data_fleetx-repla_date      = 'X'.
  gwa_data_fleetx-repla_odom      = 'X'.
  gwa_data_fleetx-repla_oph       = 'X'.
  gwa_data_fleetx-fleet_use       = 'X'.
  gwa_data_fleetx-card_num        = 'X'.
  gwa_data_fleetx-max_occupants   = 'X'.
  gwa_data_fleetx-key_num         = 'X'.
  gwa_data_fleetx-num_axle        = 'X'.
  gwa_data_fleetx-engine_type     = 'X'.
  gwa_data_fleetx-engine_snr      = 'X'.
  gwa_data_fleetx-speed_max       = 'X'.
  gwa_data_fleetx-speed_unit      = 'X'.
  gwa_data_fleetx-engine_power    = 'X'.
  gwa_data_fleetx-unit_power      = 'X'.
  gwa_data_fleetx-revolutions     = 'X'.
  gwa_data_fleetx-engine_cap      = 'X'.
  gwa_data_fleetx-unit_cap        = 'X'.
  gwa_data_fleetx-engine_cyl      = 'X'.
  gwa_data_fleetx-fuel_pri        = 'X'.
  gwa_data_fleetx-fuel_sec        = 'X'.
  gwa_data_fleetx-oil_type        = 'X'.
  gwa_data_fleetx-pri_calc        = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_LOG_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_log_erros .

  DATA: go_table   TYPE REF TO cl_salv_table,
        lo_columns TYPE REF TO cl_salv_columns_table,
        lo_column  TYPE REF TO cl_salv_column.


  IF git_erros[] IS NOT INITIAL.
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = go_table
          CHANGING
            t_table        = git_erros[] ).
      CATCH cx_salv_msg.
    ENDTRY.

    "Layout
    DATA(lo_functions) = go_table->get_functions( ).
    lo_functions->set_all( abap_true ).
    lo_functions->set_export_send( abap_false ).

    "Seleciona mais de uma linha (BOX)
    DATA(lo_select) = go_table->get_selections( ).
    lo_select->set_selection_mode( 3 ).

    "Zebra
    DATA(lo_display) = go_table->get_display_settings( ).
    lo_display->set_striped_pattern( abap_true ).

    "Ajusta as Colunas
    lo_columns = go_table->get_columns( ).
    lo_columns->set_optimize( 'X' ).

    lo_column = lo_columns->get_column('LINHA').
    lo_column->set_short_text( 'Linha' ).

    lo_column = lo_columns->get_column( 'ID' ).
    lo_column->set_short_text( 'ID' ).

    lo_column = lo_columns->get_column( 'TYPE' ).
    lo_column->set_short_text( 'Tipo' ).

    lo_column = lo_columns->get_column( 'NUMBER' ).
    lo_column->set_short_text( 'Número' ).

    lo_column = lo_columns->get_column( 'MESSAGE' ).
    lo_column->set_short_text( 'Mensagem 1' ).

    lo_column = lo_columns->get_column( 'MESSAGE_V1' ).
    lo_column->set_short_text( 'Mensagem 2' ).

    lo_column = lo_columns->get_column( 'MESSAGE_V2' ).
    lo_column->set_short_text( 'Mensagem 3' ).

    lo_column = lo_columns->get_column( 'MESSAGE_V3' ).
    lo_column->set_short_text( 'Mensagem 4' ).

    go_table->set_screen_popup(
      start_column = 1
      end_column   = 170
      start_line   = 1
      end_line     = 22 ).

    go_table->display( ).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0110 OUTPUT.
  SET PF-STATUS 'ST-0110'.
  SET TITLEBAR 'TITLE0110'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_SET_DADOS_EQUIPAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_set_dados_equipamento .
  DATA: lva_ctrlcol1 TYPE alsmex_tabline-value,
        lva_arbpl    TYPE crhd-arbpl,
        lwa_crhd     TYPE crhd.

  DATA: lit_datxls_01     TYPE TABLE OF ty_saida_cat01 WITH HEADER LINE.
  DATA: lit_datxls_02     TYPE TABLE OF ty_saida_cat02 WITH HEADER LINE.
  DATA: lit_datxls_01_m   TYPE TABLE OF ty_saida_cat01_m WITH HEADER LINE.
  DATA: lit_datxls_02_m   TYPE TABLE OF ty_saida_cat02_m WITH HEADER LINE.
  CLEAR: lit_datxls_01, lit_datxls_02.

  TRY.

      IF gva_cat = '01'.

        TRY.
            LOOP AT it_excel INTO DATA(wa_excel)."git_dados.
*      ON CHANGE OF git_dados-row.
*        lva_ctrlcol1 = git_dados-value.
*      ENDON.

              CHECK wa_excel-row NE 1 AND wa_excel-row NE 2.
              CASE wa_excel-col."git_dados-col.
                WHEN  1.   CONCATENATE wa_excel-value+6(4)  wa_excel-value+3(2) wa_excel-value+0(2) INTO lit_datxls_01-valid_date.
                WHEN  2.  lit_datxls_01-equicatgry   = wa_excel-value.
                WHEN  3.  lit_datxls_01-authgrp      = wa_excel-value.
                WHEN  4.  lit_datxls_01-obj_weight   = wa_excel-value.
                WHEN  5.  lit_datxls_01-unit_of_wt   = wa_excel-value.
                WHEN  6.  lit_datxls_01-obj_size     = wa_excel-value.

                WHEN  7.  lit_datxls_01-inventory    = wa_excel-value.
                WHEN  8.  CONCATENATE wa_excel-value+6(4)  wa_excel-value+3(2) wa_excel-value+0(2) INTO lit_datxls_01-start_from.
                WHEN  9.  lit_datxls_01-objecttype   = wa_excel-value.
                WHEN  10. lit_datxls_01-acquisval    = wa_excel-value.
                WHEN  11. lit_datxls_01-currency     = wa_excel-value.
                WHEN  12. CONCATENATE wa_excel-value+6(4)  wa_excel-value+3(2) wa_excel-value+0(2) INTO  lit_datxls_01-acqdate.
                WHEN  13. lit_datxls_01-manfacture   = wa_excel-value.
                WHEN  14. lit_datxls_01-mancountry   = wa_excel-value.
                WHEN  15. lit_datxls_01-manmodel     = wa_excel-value.
                WHEN  16. lit_datxls_01-constyear    = wa_excel-value.
                WHEN  17. lit_datxls_01-constmonth   = wa_excel-value.
                WHEN  18. lit_datxls_01-manparno     = wa_excel-value.
                WHEN  19. lit_datxls_01-manserno     = wa_excel-value.
                WHEN  20. lit_datxls_01-descript     = wa_excel-value.
                WHEN  21. lit_datxls_01-datab        = wa_excel-value.
                WHEN  22. lit_datxls_01-maintplant   = wa_excel-value.
                WHEN  23. lit_datxls_01-maintloc     = wa_excel-value.
                WHEN  24. lit_datxls_01-maintroom    = wa_excel-value.
                WHEN  25. lit_datxls_01-plsectn      = wa_excel-value.
                WHEN  26. lit_datxls_01-pp_wkctr     = wa_excel-value..
                WHEN  27. lit_datxls_01-abcindic     = wa_excel-value.
                WHEN  28. lit_datxls_01-sortfield    = wa_excel-value.
                WHEN  29. lit_datxls_01-comp_code    = wa_excel-value.
                WHEN  30. lit_datxls_01-bus_area     = wa_excel-value.
                WHEN  31. lit_datxls_01-asset_no     = wa_excel-value.
                WHEN  32. lit_datxls_01-planplant    = wa_excel-value.
                WHEN  33. lit_datxls_01-sub_number   = wa_excel-value.
                WHEN  34. lit_datxls_01-costcenter   = wa_excel-value.
                WHEN  35. lit_datxls_01-wbs_elem     = wa_excel-value.
*        WHEN  36. lit_datxls_01-standorder   = wa_excel-value.
*        WHEN  37. lit_datxls_01-settlorder   = wa_excel-value.
                WHEN  36. lit_datxls_01-plangroup    = wa_excel-value.
                WHEN  37.
                  lva_arbpl = wa_excel-value.
                  SELECT SINGLE *
                    FROM crhd
                    INTO lwa_crhd
                      WHERE arbpl EQ  lva_arbpl
                 AND werks EQ lit_datxls_01-maintplant.
                  lit_datxls_01-work_ctr = lwa_crhd-objid.
                  CLEAR: lwa_crhd, lva_arbpl.

                WHEN  38. lit_datxls_01-catprofile   = wa_excel-value.
                WHEN  39. lit_datxls_01-wergw        = wa_excel-value.
                WHEN  40. lit_datxls_01-datum        = wa_excel-value.
                WHEN  41. lit_datxls_01-uzeit        = wa_excel-value.
                WHEN  42. lit_datxls_01-read_floc    = wa_excel-value.
                WHEN  43. lit_datxls_01-consttype    = wa_excel-value.
                WHEN  44. lit_datxls_01-inst_pos     = wa_excel-value.
                WHEN  45. lit_datxls_01-techid       = wa_excel-value.


              ENDCASE.
              AT END OF row.
                ADD 1 TO lit_datxls_01-id.
                APPEND lit_datxls_01.
              ENDAT.
            ENDLOOP.
          CATCH cx_sy_conversion_overflow.
            MESSAGE 'Não foi possível carregar as informações' TYPE 'E' DISPLAY LIKE 'I'.
        ENDTRY.

        git_saida_cat01[] = lit_datxls_01[].

      ELSE.
        SORT it_excel BY row col.
        CLEAR: wa_excel.
        TRY.
            LOOP AT it_excel INTO wa_excel."git_dados.
*      ON CHANGE OF git_dados-row.
*        lva_ctrlcol1 = git_dados-value.
*      ENDON.

              CHECK wa_excel-row NE 1 AND wa_excel-row NE 2.
              CASE wa_excel-col."git_dados-col.
                WHEN  1.     lit_datxls_02-external_number  = wa_excel-value.
                WHEN  2.    CONCATENATE wa_excel-value+6(4)  wa_excel-value+3(2) wa_excel-value+0(2) INTO lit_datxls_02-valid_date.
                WHEN  3.     lit_datxls_02-equicatgry             = wa_excel-value.
                WHEN  4.     lit_datxls_02-authgrp                = wa_excel-value.
                WHEN  5.     lit_datxls_02-obj_weight             = wa_excel-value.
                WHEN  6.     lit_datxls_02-unit_of_wt             = wa_excel-value.
                WHEN  7.
                  REPLACE ALL OCCURRENCES OF ',' IN wa_excel-value WITH '.'.
                  lit_datxls_02-obj_size  = wa_excel-value.
                WHEN  8.     lit_datxls_02-inventory              = wa_excel-value.
                WHEN  9.     CONCATENATE wa_excel-value+6(4)  wa_excel-value+3(2) wa_excel-value+0(2) INTO lit_datxls_02-start_from.
                WHEN  10.    lit_datxls_02-objecttype             = wa_excel-value.
                WHEN  11.    lit_datxls_02-acquisval              = wa_excel-value.
                WHEN  12.    lit_datxls_02-currency               = wa_excel-value.
                WHEN  13.    CONCATENATE wa_excel-value+6(4)  wa_excel-value+3(2) wa_excel-value+0(2) INTO lit_datxls_02-acqdate.
                WHEN  14.    lit_datxls_02-manfacture             = wa_excel-value.
                WHEN  15.    lit_datxls_02-mancountry             = wa_excel-value.
                WHEN  16.    lit_datxls_02-manmodel               = wa_excel-value.
                WHEN  17.    lit_datxls_02-constyear              = wa_excel-value.
                WHEN  18.    lit_datxls_02-constmonth             = wa_excel-value.
                WHEN  19.    lit_datxls_02-manparno               = wa_excel-value.
                WHEN  20.    lit_datxls_02-manserno               = wa_excel-value.
                WHEN  21.    lit_datxls_02-descript               = wa_excel-value.
                WHEN  22.    CONCATENATE wa_excel-value+6(4)  wa_excel-value+3(2) wa_excel-value+0(2) INTO lit_datxls_02-datab.
                WHEN  23.    lit_datxls_02-maintplant             = wa_excel-value.
                WHEN  24.    lit_datxls_02-maintloc               = wa_excel-value.
                WHEN  25.    lit_datxls_02-maintroom              = wa_excel-value.
                WHEN  26.    lit_datxls_02-plsectn                = wa_excel-value.
                WHEN  27.    lit_datxls_02-pp_wkctr               = wa_excel-value.
                WHEN  28.    lit_datxls_02-abcindic               = wa_excel-value.
                WHEN  29.    lit_datxls_02-sortfield              = wa_excel-value.
                WHEN  30.    lit_datxls_02-comp_code              = wa_excel-value.
                WHEN  31.    lit_datxls_02-bus_area               = wa_excel-value.
                WHEN  32.    lit_datxls_02-asset_no               = wa_excel-value.
                WHEN  33.    lit_datxls_02-costcenter             = wa_excel-value.
                WHEN  34.    lit_datxls_02-wbs_elem               = wa_excel-value.
*        WHEN  35.    lit_datxls_02-standorder             = wa_excel-value. --Ordem permanente combustivel
*        WHEN  36.    lit_datxls_02-settlorder             = wa_excel-value. --Ordem de apropriação / Lubrificação
                WHEN  35.    lit_datxls_02-planplant              = wa_excel-value.
                WHEN  36.    lit_datxls_02-plangroup              = wa_excel-value.

                WHEN  37.
                  lva_arbpl = git_dados-value.
                  SELECT SINGLE *
                    FROM crhd
                    INTO lwa_crhd
                      WHERE arbpl EQ lva_arbpl
                 AND werks EQ lit_datxls_01-maintplant.
                  lit_datxls_02-work_ctr = lwa_crhd-objid.
                  CLEAR: lwa_crhd, lva_arbpl.

                WHEN  38.   lit_datxls_02-catprofile             = wa_excel-value.
                WHEN  39.   lit_datxls_02-read_floc              = wa_excel-value.
                WHEN  40.   CONCATENATE wa_excel-value+6(4)  wa_excel-value+3(2) wa_excel-value+0(2) INTO lit_datxls_02-datum.
                WHEN  41.   lit_datxls_02-uzeit                 = wa_excel-value.
                WHEN  42.   lit_datxls_02-consttype             = wa_excel-value.
                WHEN  43.   lit_datxls_02-inst_pos              = wa_excel-value.
                WHEN  44.   lit_datxls_02-techid                = wa_excel-value.
                WHEN  45.   lit_datxls_02-fleet_num             = wa_excel-value.
                WHEN  46.   lit_datxls_02-license_num           = wa_excel-value.
                WHEN  47.   CONCATENATE wa_excel-value+6(4)  wa_excel-value+3(2) wa_excel-value+0(2) INTO lit_datxls_02-expiry_date.
                WHEN  48.   lit_datxls_02-fleet_vin             = wa_excel-value.
                WHEN  49.   lit_datxls_02-chassis_num           = wa_excel-value.
                WHEN  50.   lit_datxls_02-gross_wgt             = wa_excel-value.
                WHEN  51.   lit_datxls_02-load_wgt              = wa_excel-value.
                WHEN  52.   lit_datxls_02-load_vol              = wa_excel-value.
                WHEN  53.   lit_datxls_02-vol_unit              = wa_excel-value.
                WHEN  54.   lit_datxls_02-load_hgt              = wa_excel-value.
                WHEN  55.   lit_datxls_02-load_dim_unit         = wa_excel-value.
                WHEN  56.   lit_datxls_02-load_wid              = wa_excel-value.
                WHEN  57.   lit_datxls_02-load_len              = wa_excel-value.
                WHEN  58.   lit_datxls_02-no_compart            = wa_excel-value.
                WHEN  59.
                  REPLACE ALL OCCURRENCES OF ',' IN wa_excel-value WITH '.'.
                  lit_datxls_02-fleet_hgt             = wa_excel-value.
                WHEN  60.   lit_datxls_02-dim_unit              = wa_excel-value.
                WHEN  61.
                  REPLACE ALL OCCURRENCES OF ',' IN wa_excel-value WITH '.'.
                  lit_datxls_02-fleet_wid             = wa_excel-value.
                WHEN  62.
                  REPLACE ALL OCCURRENCES OF ',' IN wa_excel-value WITH '.'.
                  lit_datxls_02-fleet_len             = wa_excel-value.
                WHEN  63.   CONCATENATE wa_excel-value+6(4)  wa_excel-value+3(2) wa_excel-value+0(2) INTO lit_datxls_02-repla_date.
                WHEN  64.   lit_datxls_02-repla_odom            = wa_excel-value.
                WHEN  65.   lit_datxls_02-repla_oph             = wa_excel-value.
                WHEN  66.   lit_datxls_02-fleet_use             = wa_excel-value.
                WHEN  67.   lit_datxls_02-card_num              = wa_excel-value.
                WHEN  68.   lit_datxls_02-max_occupants         = wa_excel-value.
                WHEN  69.   lit_datxls_02-key_num               = wa_excel-value.
                WHEN  70.   lit_datxls_02-num_axle              = wa_excel-value.
                WHEN  71.   lit_datxls_02-engine_type           = wa_excel-value.
                WHEN  72.   lit_datxls_02-engine_snr            = wa_excel-value.
                WHEN  73.   lit_datxls_02-speed_max             = wa_excel-value.
                WHEN  74.   lit_datxls_02-speed_unit            = wa_excel-value.
                WHEN  75.   lit_datxls_02-engine_power          = wa_excel-value.
                WHEN  76.   lit_datxls_02-unit_power            = wa_excel-value.
                WHEN  77.   lit_datxls_02-revolutions           = wa_excel-value.
                WHEN  78.   lit_datxls_02-engine_cap            = wa_excel-value.
                WHEN  79.   lit_datxls_02-unit_cap              = wa_excel-value.
                WHEN  80.   lit_datxls_02-engine_cyl            = wa_excel-value.
                WHEN  81.   lit_datxls_02-fuel_pri              = wa_excel-value.
                WHEN  82.   lit_datxls_02-fuel_sec              = wa_excel-value.
                WHEN  83.   lit_datxls_02-oil_type              = wa_excel-value.
                WHEN  84.   lit_datxls_02-pri_calc              = wa_excel-value.
                WHEN  85.   lit_datxls_02-div1                  = wa_excel-value.
                WHEN  86.   lit_datxls_02-tq_combustivel_1      = wa_excel-value.
                WHEN  87.   lit_datxls_02-div2                  = wa_excel-value.
                WHEN  88.   lit_datxls_02-tq_combustivel_2      = wa_excel-value.
                WHEN  89.   lit_datxls_02-div3                  = wa_excel-value.
                WHEN  90.   lit_datxls_02-tq_combustivel_3      = wa_excel-value.

              ENDCASE.
              AT END OF row.
                ADD 1 TO lit_datxls_02-id.
                APPEND lit_datxls_02.
              ENDAT.
            ENDLOOP.
          CATCH cx_sy_conversion_overflow.
            MESSAGE 'Não foi possível carregar as informações' TYPE 'E' DISPLAY LIKE 'I'.
        ENDTRY.

        git_saida_cat02[] = lit_datxls_02[].
      ENDIF.
    CATCH cx_sy_conversion_no_number INTO DATA(erro_arquivo) .
      MESSAGE 'Não foi possível carregar as informações' TYPE 'E' DISPLAY LIKE 'I'.
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_SET_DADOS_MOD_EQUIPAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_set_dados_mod_equipamento .

  DATA: lva_ctrlcol1 TYPE alsmex_tabline-value,
        lva_arbpl    TYPE crhd-arbpl,
        lwa_crhd     TYPE crhd.

  DATA: lit_datxls_01     TYPE TABLE OF ty_saida_cat01 WITH HEADER LINE.
  DATA: lit_datxls_02     TYPE TABLE OF ty_saida_cat02 WITH HEADER LINE.
  DATA: lit_datxls_01_m   TYPE TABLE OF ty_saida_cat01_m WITH HEADER LINE.
  DATA: lit_datxls_02_m   TYPE TABLE OF ty_saida_cat02_m WITH HEADER LINE.

  CLEAR: lit_datxls_01_m, lit_datxls_02_m.

  TRY.
      IF gva_cat = '01'.

        TRY.
            LOOP AT it_excel INTO DATA(wa_excel). "git_dados.
*      ON CHANGE OF git_dados-row.
*        lva_ctrlcol1 = git_dados-value.
*      ENDON.
              CHECK wa_excel-row NE 1 AND wa_excel-row NE 2.
              TRY.
                  CASE wa_excel-col."git_dados-col.
                    WHEN  1.  lit_datxls_01_m-equipment = wa_excel-value.
                    WHEN  2.  CONCATENATE wa_excel-value+6(4)  wa_excel-value+3(2) wa_excel-value+0(2) INTO lit_datxls_01_m-valid_date.
                    WHEN  3.  lit_datxls_01_m-equicatgry   = wa_excel-value.
                    WHEN  4.  lit_datxls_01_m-authgrp      = wa_excel-value.
                    WHEN  5.  lit_datxls_01_m-obj_weight   = wa_excel-value.
                    WHEN  6.
                      lit_datxls_01_m-unit_of_wt   = wa_excel-value.
                      lit_datxls_01_m-obj_size     = wa_excel-value.
                    WHEN  7.
                    WHEN  8.  lit_datxls_01_m-inventory    = wa_excel-value.
                    WHEN  9.  CONCATENATE wa_excel-value+6(4)  wa_excel-value+3(2) wa_excel-value+0(2) INTO lit_datxls_01_m-start_from.
                    WHEN  10. lit_datxls_01_m-objecttype   = wa_excel-value.
                    WHEN  11. lit_datxls_01_m-acquisval    = wa_excel-value.
                    WHEN  12. lit_datxls_01_m-currency     = wa_excel-value.
                    WHEN  13. CONCATENATE wa_excel-value+6(4)  wa_excel-value+3(2) wa_excel-value+0(2) INTO  lit_datxls_01_m-acqdate.
                    WHEN  14. lit_datxls_01_m-manfacture   = wa_excel-value.
                    WHEN  15. lit_datxls_01_m-mancountry   = wa_excel-value.
                    WHEN  16. lit_datxls_01_m-manmodel     = wa_excel-value.
                    WHEN  17. lit_datxls_01_m-constyear    = wa_excel-value.
                    WHEN  18. lit_datxls_01_m-constmonth   = wa_excel-value.
                    WHEN  19. lit_datxls_01_m-manparno     = wa_excel-value.
                    WHEN  20. lit_datxls_01_m-manserno     = wa_excel-value.
                    WHEN  21. lit_datxls_01_m-descript     = wa_excel-value.
                    WHEN  22. lit_datxls_01_m-datab        = wa_excel-value.
                    WHEN  23. lit_datxls_01_m-maintplant   = wa_excel-value.
                    WHEN  24. lit_datxls_01_m-maintloc     = wa_excel-value.
                    WHEN  25. lit_datxls_01_m-maintroom    = wa_excel-value. "Sala
                    WHEN  26. lit_datxls_01_m-plsectn      = wa_excel-value. "Área operacional
                    WHEN  27. lit_datxls_01_m-pp_wkctr     = wa_excel-value.."ID de objeto do centro de trabalho PCP
                    WHEN  28. lit_datxls_01_m-abcindic     = wa_excel-value. "Código ABC para o objeto técnico
                    WHEN  29. lit_datxls_01_m-sortfield    = wa_excel-value. "Campo de ordenação
                    WHEN  30. lit_datxls_01_m-comp_code    = wa_excel-value. "Empresa
                    WHEN  31. lit_datxls_01_m-bus_area     = wa_excel-value. "Divisão
                    WHEN  32. lit_datxls_01_m-asset_no     = wa_excel-value. "Nº principal do imobilizado
                    WHEN  33. lit_datxls_01_m-planplant    = wa_excel-value. "Centro de planejamento de manutenção
                    WHEN  34. lit_datxls_01_m-sub_number   = wa_excel-value. "Subnº do imobilizado
                    WHEN  35. lit_datxls_01_m-costcenter   = wa_excel-value. "Centro de custo
                    WHEN  36. lit_datxls_01_m-wbs_elem     = wa_excel-value. "Elemento do plano da estrutura do projeto (elemento PEP)
                    WHEN  37. lit_datxls_01_m-standorder   = wa_excel-value. "Nº ordem permanente
                    WHEN  38. lit_datxls_01_m-settlorder   = wa_excel-value. "Ordem p/apropriação de custos
                    WHEN  39. lit_datxls_01_m-plangroup    = wa_excel-value. "Grupo de planejamento para serviços cliente e manutenção


                    WHEN  40.
                      lva_arbpl = wa_excel-value.
                      SELECT SINGLE *
                        FROM crhd
                        INTO lwa_crhd
                          WHERE arbpl EQ  lva_arbpl
                            AND werks EQ lit_datxls_01_m-maintplant.
                      lit_datxls_01_m-work_ctr = lwa_crhd-objid.
                      CLEAR: lwa_crhd, lva_arbpl.

                    WHEN  41. lit_datxls_01_m-catprofile   = wa_excel-value.
                    WHEN  42. lit_datxls_01_m-wergw        = wa_excel-value.
                    WHEN  43. lit_datxls_01_m-datum        = wa_excel-value.
                    WHEN  44. lit_datxls_01_m-uzeit        = wa_excel-value.
                    WHEN  45. lit_datxls_01_m-read_floc    = wa_excel-value.
                    WHEN  46. lit_datxls_01_m-consttype    = wa_excel-value.
                    WHEN  47. lit_datxls_01_m-inst_pos     = wa_excel-value.
                    WHEN  48. lit_datxls_01_m-techid       = wa_excel-value.

                  ENDCASE.
                CATCH zcx_charg_exception .
                  MESSAGE 'Erro ao carregar os dados' TYPE 'I'.
                  EXIT.
              ENDTRY.

              AT END OF row.
                ADD 1 TO lit_datxls_01_m-id.
                APPEND lit_datxls_01_m.
              ENDAT.
            ENDLOOP.
          CATCH cx_sy_conversion_overflow.
            MESSAGE 'Não foi possível carregar as informações' TYPE 'E' DISPLAY LIKE 'I'.
        ENDTRY.

        git_saida_cat01_m[] = lit_datxls_01_m[].

      ELSE.
        CLEAR: wa_excel.
        TRY.
            LOOP AT it_excel INTO wa_excel."git_dados.
*      ON CHANGE OF git_dados-row.
*        lva_ctrlcol1 = wa_excel-value.
*      ENDON.

              CHECK wa_excel-row NE 1 AND wa_excel-row NE 2.
              CASE wa_excel-col."git_dados-col.
                WHEN  1.     lit_datxls_02_m-equipment  = wa_excel-value.
                WHEN  2.    CONCATENATE wa_excel-value+6(4)  wa_excel-value+3(2) wa_excel-value+0(2) INTO lit_datxls_02_m-valid_date.
                WHEN  3.     lit_datxls_02_m-equicatgry             = wa_excel-value.
                WHEN  4.     lit_datxls_02_m-authgrp                = wa_excel-value.
                WHEN  5.     lit_datxls_02_m-obj_weight             = wa_excel-value.
                WHEN  6.     lit_datxls_02_m-unit_of_wt             = wa_excel-value.
                WHEN  7.
                  REPLACE ALL OCCURRENCES OF ',' IN git_dados-value WITH '.'.
                  lit_datxls_02_m-obj_size  = wa_excel-value.
                WHEN  8.     lit_datxls_02_m-inventory              = wa_excel-value.
                WHEN  9.     CONCATENATE wa_excel-value+6(4)  wa_excel-value+3(2) wa_excel-value+0(2) INTO lit_datxls_02_m-start_from.
                WHEN  10.    lit_datxls_02_m-objecttype             = wa_excel-value.
                WHEN  11.    lit_datxls_02_m-acquisval              = wa_excel-value.
                WHEN  12.    lit_datxls_02_m-currency               = wa_excel-value.
                WHEN  13.    CONCATENATE wa_excel-value+6(4)  wa_excel-value+3(2) wa_excel-value+0(2) INTO lit_datxls_02_m-acqdate.
                WHEN  14.    lit_datxls_02_m-manfacture             = wa_excel-value.
                WHEN  15.    lit_datxls_02_m-mancountry             = wa_excel-value.
                WHEN  16.    lit_datxls_02_m-manmodel               = wa_excel-value.
                WHEN  17.    lit_datxls_02_m-constyear              = wa_excel-value.
                WHEN  18.    lit_datxls_02_m-constmonth             = wa_excel-value.
                WHEN  19.    lit_datxls_02_m-manparno               = wa_excel-value.
                WHEN  20.    lit_datxls_02_m-manserno               = wa_excel-value.
                WHEN  21.    lit_datxls_02_m-descript               = wa_excel-value.
                WHEN  22.    CONCATENATE wa_excel-value+6(4)  wa_excel-value+3(2) wa_excel-value+0(2) INTO lit_datxls_02_m-datab.
*                WHEN  23.    lit_datxls_02_m-maintplant             = wa_excel-value. "Centro de planejamento.
                WHEN  23.    lit_datxls_02_m-maintloc               = wa_excel-value.
                WHEN  24.    lit_datxls_02_m-maintroom              = wa_excel-value. "Sala
                WHEN  25.    lit_datxls_02_m-plsectn                = wa_excel-value. "Área operacional
                WHEN  26.    lit_datxls_02_m-pp_wkctr               = wa_excel-value.
                WHEN  27.    lit_datxls_02_m-abcindic               = wa_excel-value.
                WHEN  28.    lit_datxls_02_m-sortfield              = wa_excel-value.
*                WHEN  29.    lit_datxls_02_m-comp_code              = wa_excel-value. Empresa
*                WHEN  31.    lit_datxls_02_m-bus_area               = wa_excel-value. Divisao
                WHEN  29.    lit_datxls_02_m-asset_no               = wa_excel-value.
                WHEN  30.    lit_datxls_02_m-costcenter             = wa_excel-value.
                WHEN  31.    lit_datxls_02_m-wbs_elem               = wa_excel-value.
                WHEN  32.    lit_datxls_02_m-standorder             = wa_excel-value.
                WHEN  33.    lit_datxls_02_m-settlorder             = wa_excel-value.
*                WHEN  35.    lit_datxls_02_m-planplant              = wa_excel-value. "Centro de planejamento
                WHEN  34.    lit_datxls_02_m-plangroup              = wa_excel-value.

                WHEN  35.
                  lva_arbpl = wa_excel-value.
                  SELECT SINGLE *
                    FROM crhd
                    INTO lwa_crhd
                      WHERE arbpl EQ lva_arbpl
                        AND werks EQ lit_datxls_02_m-maintplant.
                  lit_datxls_02_m-work_ctr = lwa_crhd-objid.
                  CLEAR: lwa_crhd, lva_arbpl.

                WHEN  36.   lit_datxls_02_m-catprofile             = wa_excel-value.
*        WHEN  40.   lit_datxls_02_m-read_floc              = wa_excel-value. "Local de instalação
                WHEN  37.   CONCATENATE wa_excel-value+6(4)  wa_excel-value+3(2) wa_excel-value+0(2) INTO lit_datxls_02_m-datum.
                WHEN  38.   lit_datxls_02_m-uzeit                 = wa_excel-value.
                WHEN  39.   lit_datxls_02_m-consttype             = wa_excel-value.
                WHEN  40.   lit_datxls_02_m-inst_pos              = wa_excel-value.
                WHEN  41.   lit_datxls_02_m-techid                = wa_excel-value.
                WHEN  42.   lit_datxls_02_m-fleet_num             = wa_excel-value.
                WHEN  43.   lit_datxls_02_m-license_num           = wa_excel-value.
                WHEN  44.   CONCATENATE wa_excel-value+6(4)  wa_excel-value+3(2) wa_excel-value+0(2) INTO lit_datxls_02_m-expiry_date.
                WHEN  45.   lit_datxls_02_m-fleet_vin             = wa_excel-value.
                WHEN  46.   lit_datxls_02_m-chassis_num           = wa_excel-value.
                WHEN  47.   lit_datxls_02_m-gross_wgt             = wa_excel-value.
                WHEN  48.   lit_datxls_02_m-load_wgt              = wa_excel-value.
                WHEN  49.   lit_datxls_02_m-load_vol              = wa_excel-value.
                WHEN  50.   lit_datxls_02_m-vol_unit              = wa_excel-value.
                WHEN  51.   lit_datxls_02_m-load_hgt              = wa_excel-value.
                WHEN  52.   lit_datxls_02_m-load_dim_unit         = wa_excel-value.
                WHEN  53.   lit_datxls_02_m-load_wid              = wa_excel-value.
                WHEN  54.   lit_datxls_02_m-load_len              = wa_excel-value.
                WHEN  55.   lit_datxls_02_m-no_compart            = wa_excel-value.
                WHEN  56.
                  REPLACE ALL OCCURRENCES OF ',' IN wa_excel-value WITH '.'.
                  lit_datxls_02_m-fleet_hgt             = wa_excel-value.
                WHEN  57.   lit_datxls_02_m-dim_unit              = wa_excel-value.
                WHEN  58.
                  REPLACE ALL OCCURRENCES OF ',' IN wa_excel-value WITH '.'.
                  lit_datxls_02_m-fleet_wid             = CONV #( wa_excel-value ).
                WHEN  59.
                  REPLACE ALL OCCURRENCES OF ',' IN wa_excel-value WITH '.'.
                  lit_datxls_02_m-fleet_len             = CONV #( wa_excel-value ).
                WHEN  60.   CONCATENATE wa_excel-value+6(4)  wa_excel-value+3(2) wa_excel-value+0(2) INTO lit_datxls_02_m-repla_date.
                WHEN  61.   lit_datxls_02_m-repla_odom            = wa_excel-value.
                WHEN  62.   lit_datxls_02_m-repla_oph             = wa_excel-value.
                WHEN  63.   lit_datxls_02_m-fleet_use             = wa_excel-value.
                WHEN  64.   lit_datxls_02_m-card_num              = wa_excel-value.
                WHEN  65.   lit_datxls_02_m-max_occupants         = wa_excel-value.
                WHEN  66.   lit_datxls_02_m-key_num               = wa_excel-value.
                WHEN  67.   lit_datxls_02_m-num_axle              = wa_excel-value.
                WHEN  68.   lit_datxls_02_m-engine_type           = wa_excel-value.
                WHEN  69.   lit_datxls_02_m-engine_snr            = wa_excel-value.
                WHEN  70.   lit_datxls_02_m-speed_max             = wa_excel-value.
                WHEN  71.   lit_datxls_02_m-speed_unit            = wa_excel-value.
                WHEN  72.   lit_datxls_02_m-engine_power          = wa_excel-value.
                WHEN  73.   lit_datxls_02_m-unit_power            = wa_excel-value.
                WHEN  74.   lit_datxls_02_m-revolutions           = wa_excel-value.
                WHEN  75.   lit_datxls_02_m-engine_cap            = wa_excel-value.
                WHEN  76.   lit_datxls_02_m-unit_cap              = wa_excel-value.
                WHEN  77.   lit_datxls_02_m-engine_cyl            = wa_excel-value.
                WHEN  78.   lit_datxls_02_m-fuel_pri              = wa_excel-value.
                WHEN  79.   lit_datxls_02_m-fuel_sec              = wa_excel-value.
                WHEN  80.   lit_datxls_02_m-oil_type              = wa_excel-value.
                WHEN  81.   lit_datxls_02_m-pri_calc              = wa_excel-value.
                WHEN  82.   lit_datxls_02_m-div1                  = wa_excel-value.
                WHEN  83.   lit_datxls_02_m-tq_combustivel_1      = wa_excel-value.
                WHEN  84.   lit_datxls_02_m-div2                  = wa_excel-value.
                WHEN  85.   lit_datxls_02_m-tq_combustivel_2      = wa_excel-value.
                WHEN  86.   lit_datxls_02_m-div3                  = wa_excel-value.
                WHEN  87.   lit_datxls_02_m-tq_combustivel_3      = wa_excel-value.

              ENDCASE.
              AT END OF row.
                ADD 1 TO lit_datxls_02_m-id.
                APPEND lit_datxls_02_m.
              ENDAT.
            ENDLOOP.
          CATCH cx_sy_conversion_overflow.
            MESSAGE 'Não foi possível carregar as informações' TYPE 'E' DISPLAY LIKE 'I'.
        ENDTRY.

        git_saida_cat02_m[] = lit_datxls_02_m[].
      ENDIF.
    CATCH cx_sy_conversion_no_number INTO DATA(erro_arquivo) .
      MESSAGE 'Não foi possível carregar as informações' TYPE 'E' DISPLAY LIKE 'I'.
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0110 INPUT.
  CASE sy-ucomm.
    WHEN 'OK'.

*      CALL METHOD gob_gui_alv_grid->refresh_table_display.
      PERFORM fm_check_dados.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_CHECK_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_check_dados .
  IF c_empresa IS NOT INITIAL.
    SELECT SINGLE *
    FROM tvko
    INTO @DATA(ws_empresa)
    WHERE bukrs EQ @c_empresa.

    IF sy-subrc EQ 0.
      IF c_centro IS NOT INITIAL.

*----> CS1091710 / IR136934--->
        SELECT SINGLE *
        FROM t001k
        INTO @DATA(ws_cent)
        WHERE bukrs EQ @c_empresa
          AND bwkey EQ @c_centro.
*<---- CS1091710 / IR136934 <---
        IF sy-subrc NE 0.
          SELECT SINGLE *
          FROM t001w
          INTO @DATA(ws_cent1)
          WHERE spras EQ 'P'
            AND vkorg EQ @c_empresa
            AND werks EQ @c_centro.
        ENDIF.

        IF sy-subrc NE 0.
          MESSAGE |Centro { c_centro } não cadastrado para empresa { c_empresa }!| TYPE 'I' .
        ELSE.
          FREE: t_status[].
          PERFORM fm_carrega_arquivo.
          LEAVE TO SCREEN 0.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE |Empresa { c_empresa } não cadastrada ! | TYPE 'I' .
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_MOD_STATE_BOTTOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_mod_state_bottom .
  DATA: w_ucomm       TYPE sy-ucomm,
        active_buttom TYPE char01.
  FREE: fcode.
  CLEAR: active_buttom, w_ucomm, p_erro.

  IF git_saida_cat01[] IS NOT INITIAL OR git_saida_cat02[] IS NOT INITIAL OR git_saida_cat01_m[] IS NOT INITIAL OR git_saida_cat02_m[] IS NOT INITIAL
     OR git_saida_local_inst_001[] IS NOT INITIAL OR git_saida_local_inst_002[] IS NOT INITIAL.
    active_buttom = abap_true.
  ENDIF.

  IF active_buttom IS INITIAL.
    w_ucomm = 'EXECUTAR'.
    APPEND w_ucomm TO fcode.
    CLEAR w_ucomm.

    w_ucomm = 'VER_INFO'.
    APPEND w_ucomm TO fcode.
    CLEAR w_ucomm.

    w_ucomm = 'LIMP_ALV'.
    APPEND w_ucomm TO fcode.
    CLEAR w_ucomm.

    w_ucomm = 'ELIM_LINHA'.
    APPEND w_ucomm TO fcode.
    CLEAR w_ucomm.
  ELSE.
    w_ucomm = 'EXECUTAR'.
    APPEND w_ucomm TO fcode.
    CLEAR w_ucomm.

    IF t_status[] IS NOT INITIAL.
      LOOP AT t_status.
        IF t_status-status = '@0A@'.
          ADD 1 TO p_erro.
        ENDIF.
      ENDLOOP.

      IF p_erro IS NOT INITIAL.
        APPEND w_ucomm TO fcode.
        CLEAR w_ucomm.
      ELSE.
        DELETE fcode WHERE ucomm = 'EXECUTAR'.
      ENDIF.
*    ELSE.
*      DELETE fcode WHERE ucomm = 'EXECUTAR'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CARREGA_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_carrega_arquivo .
  "zcl_instrucao
  DATA: lwa_type   TYPE c,
        lwa_values TYPE i.

  CONSTANTS: c_ncoln LIKE sy-index VALUE 01,
             c_nline LIKE sy-index VALUE 01,
             c_path  TYPE c LENGTH 200 VALUE '/tmp/'.

  CLEAR: gva_arq, git_dados[].

  DATA(obj_excel) = NEW zcl_instrucao( ).
  CREATE OBJECT obj_excel.

  obj_excel->set_excel( obj_excel->get_excel( ) ).
  IF it_excel IS NOT INITIAL.
    PERFORM fm_proc_results.
    PERFORM fm_saida_tela.

*  IF git_dados[] IS NOT INITIAL.
*
*    PERFORM fm_proc_results.
*    PERFORM fm_saida_tela .
**      CALL SCREEN 0110 STARTING AT 8 8.---
*
**      MESSAGE 'Fim da Importação de Dados' TYPE 'I'.----
  ELSE.
    MESSAGE 'Dados não encontrados' TYPE 'E'.
  ENDIF.
*  ELSE.
*    MESSAGE 'Informar Arquivo para Importação' TYPE 'E'.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CHECK_INFORM_EQUIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_check_inform_equipc .

  DATA: lwa_erros   LIKE LINE OF git_erros,
        lva_linha   TYPE sy-tabix,
        lva_exit    TYPE c,
        lva_fun_loc TYPE  iflo-tplnr,
        lva_x       TYPE i,
        lva_string  TYPE string,
        txt         TYPE char50.

  FREE: t_status.
  CLEAR: p_erro.

  DATA:
    lit_equipment_list TYPE STANDARD TABLE OF alm_me_inventory_mng_item,
    lwa_equipment_list LIKE LINE OF lit_equipment_list.

  FIELD-SYMBOLS: <lva_field> TYPE any.

  gva_verificado = ''.
  gva_erro = ''.
  CLEAR: git_erros[].

  IF gva_cat = '01'. "Equpamento diversos.
*    PERFORM fm_criar_equipamento_diverso.
    LOOP AT  git_saida_cat01 ASSIGNING FIELD-SYMBOL(<lwa_saida_cat01>).



      CLEAR: txt.

      IF c_empresa NE <lwa_saida_cat01>-comp_code.
        txt = 'Empresa informado na planilha é diferente da empresa informada no cabeçalho'.
        APPEND VALUE #( id = <lwa_saida_cat01>-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
        <lwa_saida_cat01>-status = '@0A@'.
        p_erro = abap_true.
      ENDIF.

      IF c_centro NE <lwa_saida_cat01>-maintplant OR c_centro NE <lwa_saida_cat01>-planplant.
        txt = 'Centro de planejamento da planilha é diferente do centro informado no cabeçalho'.
        APPEND VALUE #( id = <lwa_saida_cat01>-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
        <lwa_saida_cat01>-status = '@0A@'.
        p_erro = abap_true.
      ENDIF.

      IF c_centro NE <lwa_saida_cat01>-bus_area.
        txt = 'A divisão informado na planilha é diferente do centro informado no cabeçalho'.
        APPEND VALUE #( id = <lwa_saida_cat01>-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
        <lwa_saida_cat01>-status = '@0A@'.
        p_erro = abap_true.
      ENDIF.

      IF <lwa_saida_cat01>-descript IS INITIAL.
        txt = 'Preencha a Denominação do objeto técnico'.
        APPEND VALUE #( id = <lwa_saida_cat01>-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
        <lwa_saida_cat01>-status = '@0A@'.
        p_erro = abap_true.
      ENDIF.

      IF <lwa_saida_cat01>-manmodel IS INITIAL.
        txt = 'Preencha a denominação do tipo atribuído pelo fabricante'.
        APPEND VALUE #( id = <lwa_saida_cat01>-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
        <lwa_saida_cat01>-status = '@0A@'.
        p_erro = abap_true.
      ENDIF.

      IF <lwa_saida_cat01>-equicatgry NE p_eqtyp. "Frota Agro
        txt = 'Tipo de equipamento, diferença da tela de seleção'.
        APPEND VALUE #( id = <lwa_saida_cat01>-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
        <lwa_saida_cat01>-status = '@0A@'.
        p_erro = abap_true.
      ENDIF.

      PERFORM check_local_instalacao    USING <lwa_saida_cat01>-read_floc <lwa_saida_cat01>-id <lwa_saida_cat01>-status    CHANGING txt <lwa_saida_cat01>-status.
      PERFORM check_categoria           USING <lwa_saida_cat01>-equicatgry <lwa_saida_cat01>-id <lwa_saida_cat01>-status   CHANGING txt <lwa_saida_cat01>-status.
      PERFORM check_unid_peso           USING <lwa_saida_cat01>-unit_of_wt <lwa_saida_cat01>-id <lwa_saida_cat01>-status   CHANGING txt <lwa_saida_cat01>-status.
      PERFORM check_unid_pais           USING <lwa_saida_cat01>-mancountry <lwa_saida_cat01>-id <lwa_saida_cat01>-status   CHANGING txt <lwa_saida_cat01>-status.
      PERFORM check_tipo_object         USING <lwa_saida_cat01>-objecttype <lwa_saida_cat01>-id <lwa_saida_cat01>-status   CHANGING txt <lwa_saida_cat01>-status.
      PERFORM check_grup_autor          USING <lwa_saida_cat01>-authgrp <lwa_saida_cat01>-id <lwa_saida_cat01>-status      CHANGING txt <lwa_saida_cat01>-status.
      PERFORM check_cod_moeda           USING <lwa_saida_cat01>-currency <lwa_saida_cat01>-id <lwa_saida_cat01>-status     CHANGING txt <lwa_saida_cat01>-status.
      PERFORM check_local_imobilizado   USING <lwa_saida_cat01>-maintloc <lwa_saida_cat01>-id <lwa_saida_cat01>-status     CHANGING txt <lwa_saida_cat01>-status.
      PERFORM check_area_operac         USING <lwa_saida_cat01>-plsectn    <lwa_saida_cat01>-id <lwa_saida_cat01>-status   CHANGING txt <lwa_saida_cat01>-status.
      PERFORM check_centro_trabalho     USING <lwa_saida_cat01>-pp_wkctr   <lwa_saida_cat01>-id <lwa_saida_cat01>-status   CHANGING txt <lwa_saida_cat01>-status.
      PERFORM check_codigo_abc          USING <lwa_saida_cat01>-abcindic   <lwa_saida_cat01>-id <lwa_saida_cat01>-status   CHANGING txt <lwa_saida_cat01>-status.
      PERFORM check_imobilizado         USING <lwa_saida_cat01>-asset_no   <lwa_saida_cat01>-id <lwa_saida_cat01>-status   CHANGING txt <lwa_saida_cat01>-status.
      PERFORM check_cent_custo          USING <lwa_saida_cat01>-costcenter <lwa_saida_cat01>-id <lwa_saida_cat01>-status   CHANGING txt <lwa_saida_cat01>-status.
      PERFORM check_grup_planej         USING <lwa_saida_cat01>-plangroup  <lwa_saida_cat01>-id <lwa_saida_cat01>-status   CHANGING txt <lwa_saida_cat01>-status.
      PERFORM check_perf_catalago       USING <lwa_saida_cat01>-catprofile <lwa_saida_cat01>-id <lwa_saida_cat01>-status   CHANGING txt <lwa_saida_cat01>-status.
      PERFORM check_conjunto            USING <lwa_saida_cat01>-consttype  <lwa_saida_cat01>-id <lwa_saida_cat01>-status   CHANGING txt <lwa_saida_cat01>-status.

      IF p_erro IS INITIAL.
        txt = 'Erros não encontrado'.
        APPEND VALUE #( id = <lwa_saida_cat01>-id status = '@08@' type = 'S' desc_status = txt ) TO t_status.
        <lwa_saida_cat01>-status = '@08@'.
      ENDIF.
      CLEAR: p_erro.
    ENDLOOP.

  ELSE.
    IF gva_cat = '02'. "Equipamento Frota.
*      PERFORM fm_criar_equipamento_frota.
      CLEAR: p_erro.
      LOOP AT  git_saida_cat02 ASSIGNING FIELD-SYMBOL(<lwa_saida_cat02>).
        IF c_empresa NE <lwa_saida_cat02>-comp_code.
          txt = 'Empresa informado na planilha é diferente da empresa informada no cabeçalho'.
          APPEND VALUE #( id = <lwa_saida_cat02>-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          <lwa_saida_cat02>-status = '@0A@'.
          p_erro = abap_true.
        ENDIF.

        IF c_centro NE <lwa_saida_cat02>-maintplant OR c_centro NE <lwa_saida_cat02>-planplant.
          txt = 'Centro de planejamento da planilha é diferente do centro informado no cabeçalho'.
          APPEND VALUE #( id = <lwa_saida_cat02>-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          <lwa_saida_cat02>-status = '@0A@'.
          p_erro = abap_true.
        ENDIF.

        IF c_centro NE <lwa_saida_cat02>-bus_area.
          txt = 'A divisão informado na planilha é diferente do centro informado no cabeçalho'.
          APPEND VALUE #( id = <lwa_saida_cat02>-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          <lwa_saida_cat02>-status = '@0A@'.
          p_erro = abap_true.
        ENDIF.

        CLEAR: txt.

        IF <lwa_saida_cat02>-descript IS INITIAL.
          txt = 'Preencha a Denominação do objeto técnico'.
          APPEND VALUE #( id = <lwa_saida_cat02>-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          <lwa_saida_cat02>-status = '@0A@'.
          p_erro = abap_true.
        ENDIF.

        IF <lwa_saida_cat02>-manmodel IS INITIAL.
          txt = 'Preencha a denominação do tipo atribuído pelo fabricante'.
          APPEND VALUE #( id = <lwa_saida_cat02>-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          <lwa_saida_cat02>-status = '@0A@'.
          p_erro = abap_true.
        ENDIF.

        IF <lwa_saida_cat02>-equicatgry NE p_eqtyp. "Frota Agro
          txt = 'Tipo de equipamento, diferença da tela de seleção'.
          APPEND VALUE #( id = <lwa_saida_cat02>-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          <lwa_saida_cat02>-status = '@0A@'.
          p_erro = abap_true.
        ENDIF.


        IF <lwa_saida_cat02>-equicatgry EQ 'V'. "Frota Agro
          IF <lwa_saida_cat02>-external_number IS INITIAL.
            txt = 'Preencha o numero externo equipamento'.
            APPEND VALUE #( id = <lwa_saida_cat02>-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            <lwa_saida_cat02>-status = '@0A@'.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        PERFORM check_local_instalacao    USING <lwa_saida_cat02>-read_floc <lwa_saida_cat02>-id <lwa_saida_cat02>-status    CHANGING txt <lwa_saida_cat02>-status.
        PERFORM check_categoria           USING <lwa_saida_cat02>-equicatgry <lwa_saida_cat02>-id <lwa_saida_cat02>-status   CHANGING txt <lwa_saida_cat02>-status.
        PERFORM check_unid_peso           USING <lwa_saida_cat02>-unit_of_wt <lwa_saida_cat02>-id <lwa_saida_cat02>-status   CHANGING txt <lwa_saida_cat02>-status.
        PERFORM check_unid_pais           USING <lwa_saida_cat02>-mancountry <lwa_saida_cat02>-id <lwa_saida_cat02>-status   CHANGING txt <lwa_saida_cat02>-status.
        PERFORM check_tipo_object         USING <lwa_saida_cat02>-objecttype <lwa_saida_cat02>-id <lwa_saida_cat02>-status   CHANGING txt <lwa_saida_cat02>-status.
        PERFORM check_grup_autor          USING <lwa_saida_cat02>-authgrp <lwa_saida_cat02>-id <lwa_saida_cat02>-status      CHANGING txt <lwa_saida_cat02>-status.
        PERFORM check_cod_moeda           USING <lwa_saida_cat02>-currency <lwa_saida_cat02>-id <lwa_saida_cat02>-status     CHANGING txt <lwa_saida_cat02>-status.
        PERFORM check_local_imobilizado   USING <lwa_saida_cat02>-maintloc <lwa_saida_cat02>-id <lwa_saida_cat02>-status     CHANGING txt <lwa_saida_cat02>-status.
        PERFORM check_area_operac         USING <lwa_saida_cat02>-plsectn    <lwa_saida_cat02>-id <lwa_saida_cat02>-status   CHANGING txt <lwa_saida_cat02>-status.
        PERFORM check_centro_trabalho     USING <lwa_saida_cat02>-pp_wkctr   <lwa_saida_cat02>-id <lwa_saida_cat02>-status   CHANGING txt <lwa_saida_cat02>-status.
        PERFORM check_codigo_abc          USING <lwa_saida_cat02>-abcindic   <lwa_saida_cat02>-id <lwa_saida_cat02>-status   CHANGING txt <lwa_saida_cat02>-status.
        PERFORM check_imobilizado         USING <lwa_saida_cat02>-asset_no   <lwa_saida_cat02>-id <lwa_saida_cat02>-status   CHANGING txt <lwa_saida_cat02>-status.
        PERFORM check_cent_custo          USING <lwa_saida_cat02>-costcenter <lwa_saida_cat02>-id <lwa_saida_cat02>-status   CHANGING txt <lwa_saida_cat02>-status.
        PERFORM check_grup_planej         USING <lwa_saida_cat02>-plangroup  <lwa_saida_cat02>-id <lwa_saida_cat02>-status   CHANGING txt <lwa_saida_cat02>-status.
        PERFORM check_perf_catalago       USING <lwa_saida_cat02>-catprofile <lwa_saida_cat02>-id <lwa_saida_cat02>-status   CHANGING txt <lwa_saida_cat02>-status.
        PERFORM check_conjunto            USING <lwa_saida_cat02>-consttype  <lwa_saida_cat02>-id <lwa_saida_cat02>-status   CHANGING txt <lwa_saida_cat02>-status.

        IF p_erro IS INITIAL.
          txt = 'Erros não encontrado'.
          APPEND VALUE #( id = <lwa_saida_cat02>-id status = '@08@' type = 'S' desc_status = txt ) TO t_status.
          <lwa_saida_cat02>-status = '@08@'.
        ENDIF.
        CLEAR: p_erro.
      ENDLOOP.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CHECK_INFORM_EQUIPC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_check_inform_equip .

  DATA: lwa_erros   LIKE LINE OF git_erros,
        lva_linha   TYPE sy-tabix,
        lva_exit    TYPE c,
        lva_fun_loc TYPE  iflo-tplnr,
        lva_x       TYPE i,
        lva_string  TYPE string,
        txt         TYPE char50.

  FREE: t_status.
  CLEAR: p_erro.

  DATA:
    lit_equipment_list TYPE STANDARD TABLE OF alm_me_inventory_mng_item,
    lwa_equipment_list LIKE LINE OF lit_equipment_list.

  FIELD-SYMBOLS: <lva_field> TYPE any.

  gva_verificado = ''.
  gva_erro = ''.
  CLEAR: git_erros[].

  IF  p_equipm = 'X'.
    IF gva_cat = '01'. "Modifica equipamento diversos.
*      PERFORM fm_fill_modifica.
*      PERFORM fm_modifica_eqpto.

      LOOP AT  git_saida_cat01_m ASSIGNING FIELD-SYMBOL(<lwa_saida_cat01_m>).
        IF <lwa_saida_cat01_m>-equipment IS INITIAL.
          txt = 'Preencha o numero do equipamento'.
          APPEND VALUE #( id = <lwa_saida_cat01_m>-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          <lwa_saida_cat01_m>-status = '@0A@'.
          p_erro = abap_true.
        ENDIF.

        IF p_erro IS INITIAL.
          txt = 'Erros não encontrado'.
          APPEND VALUE #( id = <lwa_saida_cat01_m>-id status = '@08@' type = 'S' desc_status = txt ) TO t_status.
          <lwa_saida_cat01_m>-status = '@08@'.
        ENDIF.
        CLEAR: p_erro.

      ENDLOOP.

    ELSE. "Modifica equipamento frota.
*      PERFORM fm_fill_modifica.
*      PERFORM fm_modifica.

      LOOP AT  git_saida_cat02_m ASSIGNING FIELD-SYMBOL(<lwa_saida_cat02_m>).
        IF <lwa_saida_cat02_m>-equipment IS INITIAL.
          txt = 'Preencha o numero do equipamento'.
          APPEND VALUE #( id = <lwa_saida_cat02_m>-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          <lwa_saida_cat01_m>-status = '@0A@'.
          p_erro = abap_true.
        ENDIF.

        IF p_erro IS INITIAL.
          txt = 'Erros não encontrado'.
          APPEND VALUE #( id = <lwa_saida_cat02_m>-id status = '@08@' type = 'S' desc_status = txt ) TO t_status.
          <lwa_saida_cat02_m>-status = '@08@'.
        ENDIF.
        CLEAR: p_erro.
      ENDLOOP.
    ENDIF.
  ENDIF.

*  IF git_erros[] IS NOT INITIAL.
*    PERFORM fm_log_erros.
*  ELSE.
*    gva_verificado = 'X'.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIAR_EQUIPAMENTO_DIVERSO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_criar_equipamento_diverso .

*  LOOP AT  git_saida_cat01 INTO DATA(lwa_saida_cat01).
*
*        lva_fun_loc = lwa_saida_cat01-read_floc.
*
*        lva_linha = sy-tabix.
*        CALL FUNCTION 'ALM_ME_INVENTORY_EQUI_GETLIST'
*          EXPORTING
*            func_loc       = lva_fun_loc
*          TABLES
*            equipment_list = lit_equipment_list.
*
*        IF sy-subrc EQ 0.
*
*          lwa_erros-linha      = lva_linha.
*          lwa_erros-id         = ''.
*          lwa_erros-type       = ''.
*          lwa_erros-number     = ''.
*          lwa_erros-message    = 'Local de instalação já existe equipamento montado'.
*          lwa_erros-message_v1 = lwa_saida_cat01-read_floc.
*
*          APPEND  lwa_erros TO git_erros.
*          CLEAR: lwa_erros.
*
*        ELSE.
*
*          IF lwa_saida_cat01-valid_date IS INITIAL.
*            gva_valid_date = sy-datum.
*          ELSE.
*            gva_valid_date = lwa_saida_cat01-valid_date.
*          ENDIF.
*
*          gwa_dataspecific-equicatgry  =   lwa_saida_cat01-equicatgry.
*          gwa_datageneral-authgrp      =   lwa_saida_cat01-authgrp   .
*          gwa_datageneral-obj_weight   =   lwa_saida_cat01-obj_weight .
*          gwa_datageneral-unit_of_wt   =   lwa_saida_cat01-unit_of_wt .
*          gwa_datageneral-obj_size     =   lwa_saida_cat01-obj_size  .
*          gwa_datageneral-inventory    =   lwa_saida_cat01-inventory .
*          gwa_datageneral-start_from   =   lwa_saida_cat01-start_from .
*          gwa_datageneral-objecttype   =   lwa_saida_cat01-objecttype .
*          gwa_datageneral-acquisval    =   lwa_saida_cat01-acquisval  .
*          gwa_datageneral-currency     =   lwa_saida_cat01-currency .
*          gwa_datageneral-acqdate      =   lwa_saida_cat01-acqdate   .
*          gwa_datageneral-manfacture   =   lwa_saida_cat01-manfacture .
*          gwa_datageneral-mancountry   =   lwa_saida_cat01-mancountry .
*          gwa_datageneral-manmodel     =   lwa_saida_cat01-manmodel .
*          gwa_datageneral-constyear    =   lwa_saida_cat01-constyear  .
*          gwa_datageneral-constmonth   =   lwa_saida_cat01-constmonth .
*          gwa_datageneral-manparno     =   lwa_saida_cat01-manparno  .
*          gwa_datageneral-manserno     =   lwa_saida_cat01-manserno .
*          gwa_datageneral-descript     =   lwa_saida_cat01-descript  .
*          gwa_datageneral-start_from   =   lwa_saida_cat01-start_from .
*          gwa_datageneral-maintplant   =   lwa_saida_cat01-maintplant.
*          gwa_datageneral-maintloc     =   lwa_saida_cat01-maintloc .
*          gwa_datageneral-maintroom    =   lwa_saida_cat01-maintroom . "Teste Sala - ABAP
*          gwa_datageneral-plsectn      =   lwa_saida_cat01-plsectn .
*          gwa_datageneral-pp_wkctr     =   lwa_saida_cat01-pp_wkctr .  "Verificar ABAP
*          gwa_datageneral-abcindic     =   lwa_saida_cat01-abcindic.
*          gwa_datageneral-sortfield    =   lwa_saida_cat01-sortfield  .
*          gwa_datageneral-comp_code    =   lwa_saida_cat01-comp_code .
*          gwa_datageneral-bus_area     =   lwa_saida_cat01-bus_area .
*          gwa_datageneral-asset_no     =   lwa_saida_cat01-asset_no .
*
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = gwa_datageneral-asset_no
*            IMPORTING
*              output = gwa_datageneral-asset_no.
*
*          gwa_datageneral-planplant    =   lwa_saida_cat01-planplant.
*          gwa_datageneral-sub_number   =   lwa_saida_cat01-sub_number .
*
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = gwa_datageneral-sub_number
*            IMPORTING
*              output = gwa_datageneral-sub_number.
*
*          gwa_datageneral-costcenter   =   lwa_saida_cat01-costcenter.
*
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = gwa_datageneral-costcenter
*            IMPORTING
*              output = gwa_datageneral-costcenter.
*
*          gwa_datageneral-wbs_elem     =   lwa_saida_cat01-wbs_elem .
*          gwa_datageneral-standorder   =   lwa_saida_cat01-standorder .
*
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = gwa_datageneral-standorder
*            IMPORTING
*              output = gwa_datageneral-standorder.
*
*          gwa_datageneral-settlorder   =   lwa_saida_cat01-settlorder.
*
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = gwa_datageneral-settlorder
*            IMPORTING
*              output = gwa_datageneral-settlorder.
*
*          gwa_datageneral-plangroup    =   lwa_saida_cat01-plangroup.
*          gwa_datageneral-work_ctr     =   lwa_saida_cat01-work_ctr.
*          gwa_datageneral-catprofile   =   lwa_saida_cat01-catprofile .
*          gwa_datageneral-pp_wkctr     =   lwa_saida_cat01-pp_wkctr.
*          gwa_dataspecific-read_floc   =   lwa_saida_cat01-read_floc.
*          gwa_datageneral-consttype    =   lwa_saida_cat01-consttype.
*          gwa_datainstall-funcloc      =   lwa_saida_cat01-read_floc.
*          gwa_datainstall-inst_pos     =   lwa_saida_cat01-inst_pos.
*
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = gwa_datageneral-consttype
*            IMPORTING
*              output = gwa_datageneral-consttype.
*
*          gwa_dataspecific-inst_pos    =   lwa_saida_cat01-inst_pos.
*          gwa_dataspecific-techid      =   lwa_saida_cat01-techid.
*
*          DO 47 TIMES.
*
*            CALL FUNCTION 'BAPI_EQUI_CREATE'
*              EXPORTING
*                data_general  = gwa_datageneral
*                data_specific = gwa_dataspecific
*                data_fleet    = gwa_datafleet
*                valid_date    = gva_valid_date
*                data_install  = gwa_datainstall
*              IMPORTING
*                return        = gwa_return
*              EXCEPTIONS
*                OTHERS        = 01.
*
*            IF gwa_return IS NOT INITIAL.
*              DO.
*                ASSIGN COMPONENT sy-index OF STRUCTURE gwa_datageneral TO <lva_field>.
*                IF sy-subrc IS INITIAL.
*                  MOVE <lva_field> TO lva_string.
*                  FIND ALL OCCURRENCES OF gwa_return-message_v1 IN lva_string  MATCH COUNT lva_x.
*                  IF sy-subrc = 0.
*                    <lva_field> = ''.
*                  ENDIF.
*                ELSE.
*                  EXIT.
*                ENDIF.
*              ENDDO.
*
*              lwa_erros-linha      = lva_linha.
*              lwa_erros-id         = gwa_return-id(2).
*              lwa_erros-type       = gwa_return-type.
*              lwa_erros-number     = gwa_return-number.
*              lwa_erros-message    = gwa_return-message.
*              lwa_erros-message_v1 = gwa_return-message_v1.
*              lwa_erros-message_v2 = gwa_return-message_v2.
*              lwa_erros-message_v3 = gwa_return-message_v3.
*
*              APPEND  lwa_erros TO git_erros.
*              CLEAR: lwa_erros.
*            ELSE.
*              EXIT.
*            ENDIF.
*          ENDDO.
*          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*        ENDIF.
*      ENDLOOP.


*    IF git_erros[] IS NOT INITIAL.
*      PERFORM fm_log_erros.
*    ELSE.
*      gva_verificado = 'X'.
*    ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIAR_EQUIPAMENTO_FROTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_criar_equipamento_frota .
* LOOP AT  git_saida_cat02 INTO DATA(lwa_saida_cat02).
*          lva_linha = sy-tabix.
*
*          lva_fun_loc = lwa_saida_cat02-read_floc.
*          CALL FUNCTION 'ALM_ME_INVENTORY_EQUI_GETLIST'
*            EXPORTING
*              func_loc       = lva_fun_loc
*            TABLES
*              equipment_list = lit_equipment_list.
*
*          IF sy-subrc EQ 0.
*            lwa_erros-linha      = lva_linha.
*            lwa_erros-id         = ''.
*            lwa_erros-type       = ''.
*            lwa_erros-number     = ''.
*            lwa_erros-message    = 'Local de instalação já existe equipamento montado'.
*            lwa_erros-message_v1 = lwa_saida_cat01-read_floc.
*
*            APPEND  lwa_erros TO git_erros.
*            CLEAR: lwa_erros.

*          ELSE.

*            DO 92 TIMES.
*              CALL FUNCTION 'BAPI_EQUI_CREATE'
*                EXPORTING
*                  data_general  = gwa_datageneral
*                  data_specific = gwa_dataspecific
*                  data_fleet    = gwa_datafleet
*                  valid_date    = gva_valid_date
*                  data_install  = gwa_datainstall
*                IMPORTING
*                  return        = gwa_return
*                EXCEPTIONS
*                  OTHERS        = 01.
*
*              IF gwa_return IS NOT INITIAL.
*                DO.
*                  ASSIGN COMPONENT sy-index OF STRUCTURE gwa_datageneral TO <lva_field>.
*                  IF sy-subrc IS INITIAL.
*                    MOVE <lva_field> TO lva_string.
*                    FIND ALL OCCURRENCES OF gwa_return-message_v1 IN lva_string  MATCH COUNT lva_x.
*                    IF sy-subrc = 0.
*                      <lva_field> = ''.
*                    ENDIF.
*                  ELSE.
*                    EXIT.
*                  ENDIF.
*                ENDDO.
*
*                lwa_erros-linha      = lva_linha.
*                lwa_erros-id         = gwa_return-id(2).
*                lwa_erros-type       = gwa_return-type.
*                lwa_erros-number     = gwa_return-number.
*                lwa_erros-message    = gwa_return-message.
*                lwa_erros-message_v1 = gwa_return-message_v1.
*                lwa_erros-message_v2 = gwa_return-message_v2.
*                lwa_erros-message_v3 = gwa_return-message_v3.
*
*                APPEND  lwa_erros TO git_erros.
*                CLEAR: lwa_erros.
*              ELSE.
*                EXIT.
*              ENDIF.
*            ENDDO.
*            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*          ENDIF.
*        ENDLOOP.


*    IF git_erros[] IS NOT INITIAL.
*      PERFORM fm_log_erros.
*    ELSE.
*      gva_verificado = 'X'.
*    ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEL_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN_FIELDNAME  text
*----------------------------------------------------------------------*
FORM sel_status  USING p_row p_column_fieldname.
  DATA: id        TYPE p,
        lva_linha TYPE sy-tabix,
        lwa_erros LIKE LINE OF git_erros,
        s_eqpto   TYPE equnr.


  CLEAR: id, s_eqpto.
  FREE: git_erros.
  TRY .
      IF git_saida_cat01[] IS NOT INITIAL.
        DATA(wa_cat01) = git_saida_cat01[ p_row ].
        id = wa_cat01-id.
        s_eqpto = wa_cat01-external_number.
      ENDIF.

      IF git_saida_cat02[] IS NOT INITIAL.
        DATA(wa_cat02) = git_saida_cat02[ p_row ].
        id = wa_cat02-id.
        s_eqpto = wa_cat02-external_number.
      ENDIF.

      IF git_saida_cat01_m[] IS NOT INITIAL.
        DATA(wa_cat01_m) = git_saida_cat01_m[ p_row ].
        id = wa_cat01_m-id.
        s_eqpto = wa_cat01_m-equipment.
      ENDIF.

      IF git_saida_cat02_m[] IS NOT INITIAL.
        DATA(wa_cat02_m) = git_saida_cat02_m[ p_row ].
        id = wa_cat02_m-id.
        s_eqpto = wa_cat02_m-equipment.
      ENDIF.

    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  CASE p_column_fieldname.
    WHEN 'STATUS'.
      LOOP AT t_status WHERE id = id.

        lwa_erros-linha      = lva_linha.
        lwa_erros-id         = t_status-id.
        lwa_erros-type       = t_status-type.
        lwa_erros-number     = ''.
        lwa_erros-message    = t_status-desc_status.
        lwa_erros-message_v1 = ''.

        APPEND  lwa_erros TO git_erros.
        CLEAR: lwa_erros.
      ENDLOOP.

      IF git_erros[] IS NOT INITIAL.
        PERFORM fm_log_erros.
*    ELSE.
*      gva_verificado = 'X'.
      ENDIF.

    WHEN 'EXTERNAL_NUMBER' OR 'EQUIPMENT'.
      SET PARAMETER ID 'EQN' FIELD s_eqpto.
      CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN .
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_CATEGORIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LWA_SAIDA_CAT02>_EQUICATGRY  text
*----------------------------------------------------------------------*
FORM check_categoria  USING p_equip-equicatgry TYPE ty_saida_cat02-equicatgry p_equip-id TYPE ty_saida_cat02-id p_equip-status TYPE ty_saida_cat02-status CHANGING txt TYPE char50 status TYPE char05.

  CLEAR: txt.
  IF p_equip-equicatgry IS NOT INITIAL.
    SELECT SINGLE * FROM t370t INTO @DATA(ws_t370t) WHERE eqtyp EQ @p_equip-equicatgry.
    IF sy-subrc NE 0.
      txt = |Categoria { p_equip-equicatgry } não cadastrada|.
      APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
      status = '@0A@'.
      p_erro = abap_true.
*    ELSE.
*      IF p_erro IS INITIAL.
*        txt = |Categoria validada|.
*        APPEND VALUE #( id = p_equip-id status = '@08@' type = 'S' desc_status = txt ) TO t_status.
*        status = '@08@'.
*      ENDIF.
    ENDIF.
  ELSE.
    txt = |Preencha a categoria equipamento|.
    APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
    p_erro = abap_true.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_UNID_PAIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LWA_SAIDA_CAT02>  text
*      <--P_TXT  text
*      <--P_<LWA_SAIDA_CAT02>_STATUS  text
*----------------------------------------------------------------------*
FORM check_unid_pais  USING  p_equip-mancountry TYPE ty_saida_cat02-mancountry p_equip-id TYPE ty_saida_cat02-id p_equip-status TYPE ty_saida_cat02-status CHANGING txt TYPE char50 status TYPE char05.

  IF p_equip-mancountry IS NOT INITIAL.
    SELECT SINGLE * FROM t005 INTO @DATA(ws_t005) WHERE land1 EQ @p_equip-mancountry.
    IF sy-subrc NE 0.
      txt = |Unidade do pais { p_equip-mancountry } não cadastrada|.
      APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
      status = '@0A@'.
      p_erro = abap_true.
*    ELSE.
*      IF p_erro IS INITIAL.
*        txt = |Unidade do pais validada|.
*        APPEND VALUE #( id = p_equip-id status = '@08@' type = 'S' desc_status = txt ) TO t_status.
*        status = '@08@'.
*      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_TIPO_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LWA_SAIDA_CAT02>  text
*      <--P_TXT  text
*      <--P_<LWA_SAIDA_CAT02>_STATUS  text
*----------------------------------------------------------------------*
FORM check_tipo_object  USING p_equip-objecttype TYPE ty_saida_cat02-objecttype p_equip-id TYPE ty_saida_cat02-id p_equip-status TYPE ty_saida_cat02-status CHANGING txt TYPE char50 status TYPE char05.
  DATA: i_eqart   TYPE itob-eqart,
        i_flt     TYPE itob_types-ind,
        i_msgty   TYPE sy-msgty,
        e_t370k   TYPE t370k,
        e_t370flt TYPE t370flt,
        e_t370k_t TYPE t370k_t.

  IF p_equip-objecttype IS NOT INITIAL.

    CALL FUNCTION 'ITOB_CHECK_EQUITYPE'
      EXPORTING
        eqart_imp         = p_equip-objecttype
        fleet_mode        = i_flt
        dialog_cursor     = 'ITOB-EQART'
        x_mess_type       = i_msgty
      IMPORTING
        t370k_exp         = e_t370k
        t370flt_exp       = e_t370flt
        t370k_t_exp       = e_t370k_t
      EXCEPTIONS
        application_error = 2
        OTHERS            = 3.

    IF sy-subrc NE 0.
      txt = |Objeto técnico { p_equip-objecttype } não cadastrada|.
      APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
      status = '@0A@'.
      p_erro = abap_true.
*    ELSE.
*      IF p_erro IS INITIAL.
*        txt = |Objeto técnico validado|.
*        APPEND VALUE #( id = p_equip-id status = '@08@' type = 'S' desc_status = txt ) TO t_status.
*        status = '@08@'.
*      ENDIF.
    ENDIF.
  ELSE.
    txt = |Preencher objeto técnico |.
    APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
    p_erro = abap_true.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_GRUP_AUTOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LWA_SAIDA_CAT02>  text
*      <--P_TXT  text
*      <--P_<LWA_SAIDA_CAT02>_STATUS  text
*----------------------------------------------------------------------*
FORM check_grup_autor  USING p_equip-authgrp TYPE ty_saida_cat02-authgrp p_equip-id TYPE ty_saida_cat02-id p_equip-status TYPE ty_saida_cat02-status CHANGING txt TYPE char50 status TYPE char05.

  IF p_equip-authgrp IS NOT INITIAL.
    SELECT SINGLE * FROM t370b INTO @DATA(ws_t370b) WHERE begru EQ @p_equip-authgrp.
    IF sy-subrc NE 0.
      txt = |Grupo de autorização { p_equip-authgrp  } não cadastrado|.
      APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
      status = '@0A@'.
      p_erro = abap_true.
*    ELSE.
*      IF p_erro IS INITIAL.
*        txt = |Grupo de autorização validado|.
*        APPEND VALUE #( id = p_equip-id status = '@08@' type = 'S' desc_status = txt ) TO t_status.
*        status = '@08@'.
*      ENDIF.
    ENDIF.
  ELSE.
    txt = |Preencha o grupo de autorização|.
    APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
    p_erro = abap_true.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_COD_MOEDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LWA_SAIDA_CAT02>  text
*      <--P_TXT  text
*      <--P_<LWA_SAIDA_CAT02>_STATUS  text
*----------------------------------------------------------------------*
FORM check_cod_moeda  USING p_equip-currency TYPE ty_saida_cat02-currency p_equip-id TYPE ty_saida_cat02-id p_equip-status TYPE ty_saida_cat02-status CHANGING txt TYPE char50 status TYPE char05.
  IF p_equip-currency IS NOT INITIAL.
    SELECT SINGLE * FROM tcurc INTO @DATA(ws_tcurc) WHERE waers EQ @p_equip-currency.
    IF sy-subrc NE 0.
      txt = |Código da moéda { p_equip-currency  } não cadastrado|.
      APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
      status = '@0A@'.
      p_erro = abap_true.
*    ELSE.
*      IF p_erro IS INITIAL.
*        txt = |Código da moéda validado|.
*        APPEND VALUE #( id = p_equip-id status = '@08@' type = 'S' desc_status = txt ) TO t_status.
*        status = '@08@'.
*      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_LOCAL_IMOBILIZADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LWA_SAIDA_CAT02>  text
*      <--P_TXT  text
*      <--P_<LWA_SAIDA_CAT02>_STATUS  text
*----------------------------------------------------------------------*
FORM check_local_imobilizado  USING p_equip-maintloc  TYPE ty_saida_cat02-maintloc  p_equip-id TYPE ty_saida_cat02-id p_equip-status TYPE ty_saida_cat02-status CHANGING txt TYPE char50 status TYPE char05.
  IF p_equip-maintloc IS NOT INITIAL.
    SELECT SINGLE * FROM t499s INTO @DATA(ws_t499s) WHERE werks EQ @c_centro AND stand EQ @p_equip-maintloc.
    IF sy-subrc NE 0.
      txt = |Localização do imobilizado { p_equip-maintloc } não cadastrado|.
      APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
      status = '@0A@'.
      p_erro = abap_true.
*    ELSE.
*      IF p_erro IS INITIAL.
*        txt = |Fabricante do imobilizado validado|.
*        APPEND VALUE #( id = p_equip-id status = '@08@' type = 'S' desc_status = txt ) TO t_status.
*        status = '@08@'.
*      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_AREA_OPERAC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LWA_SAIDA_CAT02>  text
*      <--P_TXT  text
*      <--P_<LWA_SAIDA_CAT02>_STATUS  text
*----------------------------------------------------------------------*
FORM check_area_operac  USING  p_equip-plsectn TYPE ty_saida_cat02-plsectn p_equip-id TYPE ty_saida_cat02-id p_equip-status TYPE ty_saida_cat02-status CHANGING txt TYPE char50 status TYPE char05.
  IF p_equip-plsectn  IS NOT INITIAL.
    p_equip-plsectn = |{ p_equip-plsectn ALPHA = IN }|.
    SELECT SINGLE * FROM t357 INTO @DATA(ws_t357) WHERE werks EQ @c_centro AND beber EQ @p_equip-plsectn.
    IF sy-subrc NE 0.
      txt = |Area operacional { p_equip-plsectn } não cadastrado|.
      APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
      status = '@0A@'.
      p_erro = abap_true.
*    ELSE.
*      IF p_erro IS INITIAL.
*        txt = |Area operacional validado|.
*        APPEND VALUE #( id = p_equip-id status = '@08@' type = 'S' desc_status = txt ) TO t_status.
*        status = '@08@'.
*      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_CENTRO_TRABALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LWA_SAIDA_CAT02>  text
*      <--P_TXT  text
*      <--P_<LWA_SAIDA_CAT02>_STATUS  text
*----------------------------------------------------------------------*
FORM check_centro_trabalho  USING  p_equip-pp_wkctr TYPE ty_saida_cat02-pp_wkctr p_equip-id TYPE ty_saida_cat02-id p_equip-status TYPE ty_saida_cat02-status CHANGING txt TYPE char50 status TYPE char05.
  IF p_equip-pp_wkctr  IS NOT INITIAL.

    SELECT SINGLE * FROM m_cramv INTO @DATA(ws_m_cramv) WHERE werks EQ @c_centro AND arbpl EQ @p_equip-pp_wkctr.
    IF sy-subrc NE 0.
      txt = |Centro de trabalho { p_equip-pp_wkctr } não cadastrado|.
      APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
      status = '@0A@'.
      p_erro = abap_true.
*    ELSE.
*      IF p_erro IS INITIAL.
*        txt = |Centro de trabalho validado|.
*        APPEND VALUE #( id = p_equip-id status = '@08@' type = 'S' desc_status = txt ) TO t_status.
*        status = '@08@'.
*      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_CODIGO_ABC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LWA_SAIDA_CAT02>  text
*      <--P_TXT  text
*      <--P_<LWA_SAIDA_CAT02>_STATUS  text
*----------------------------------------------------------------------*
FORM check_codigo_abc  USING p_equip-abcindic TYPE ty_saida_cat02-abcindic p_equip-id TYPE ty_saida_cat02-id p_equip-status TYPE ty_saida_cat02-status CHANGING txt TYPE char50 status TYPE char05.
  IF p_equip-abcindic IS NOT INITIAL.

    SELECT SINGLE * FROM t370c_t INTO @DATA(ws_t370c_t) WHERE spras EQ @sy-langu AND abckz EQ @p_equip-abcindic .
    IF sy-subrc NE 0.
      txt = |Código ABC { p_equip-abcindic } não cadastrado|.
      APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
      status = '@0A@'.
      p_erro = abap_true.
*    ELSE.
*      IF p_erro IS INITIAL.
*        txt = |Código ABC validado|.
*        APPEND VALUE #( id = p_equip-id status = '@08@' type = 'S' desc_status = txt ) TO t_status.
*        status = '@08@'.
*      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_IMOBILIZADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LWA_SAIDA_CAT02>  text
*      <--P_TXT  text
*      <--P_<LWA_SAIDA_CAT02>_STATUS  text
*----------------------------------------------------------------------*
FORM check_imobilizado  USING p_equip-asset_no TYPE ty_saida_cat02-asset_no p_equip-id TYPE ty_saida_cat02-id p_equip-status TYPE ty_saida_cat02-status CHANGING txt TYPE char50 status TYPE char05.
  IF p_equip-asset_no IS NOT INITIAL.
    p_equip-asset_no = |{ p_equip-asset_no ALPHA = IN }|.
    SELECT SINGLE * FROM anla INTO @DATA(ws_anla) WHERE anln1 EQ @p_equip-asset_no AND bukrs EQ @c_empresa.
    IF sy-subrc NE 0.
      txt = |Imobilizado { p_equip-asset_no } não cadastrado|.
      APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
      status = '@0A@'.
      p_erro = abap_true.
*    ELSE.
*      IF p_erro IS INITIAL.
*        txt = |Imobilizado validado|.
*        APPEND VALUE #( id = p_equip-id status = '@08@' type = 'S' desc_status = txt ) TO t_status.
*        status = '@08@'.
*      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_CENT_CUSTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LWA_SAIDA_CAT02>  text
*      <--P_TXT  text
*      <--P_<LWA_SAIDA_CAT02>_STATUS  text
*----------------------------------------------------------------------*
FORM check_cent_custo  USING p_equip-costcenter TYPE ty_saida_cat02-costcenter p_equip-id TYPE ty_saida_cat02-id p_equip-status TYPE ty_saida_cat02-status CHANGING txt TYPE char50 status TYPE char05.

  IF p_equip-costcenter IS NOT INITIAL.
    p_equip-costcenter = |{ p_equip-costcenter ALPHA = IN }|.
    SELECT SINGLE * FROM m_kostn INTO @DATA(ws_m_kostn) WHERE kostl  EQ @p_equip-costcenter AND spras EQ @sy-langu AND bukrs EQ @c_empresa.
    IF sy-subrc NE 0.
      txt = |Centro de custo { p_equip-costcenter } não cadastrado|.
      APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
      status = '@0A@'.
      p_erro = abap_true.
*    ELSE.
*      IF p_erro IS INITIAL.
*        txt = |Centro de custo validado|.
*        APPEND VALUE #( id = p_equip-id status = '@08@' type = 'S' desc_status = txt ) TO t_status.
*        status = '@08@'.
*      ENDIF.
    ENDIF.
  ELSE.
    txt = |Preencha o centro de custo|.
    APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
    p_erro = abap_true.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_GRUP_PLANEJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LWA_SAIDA_CAT02>  text
*      <--P_TXT  text
*      <--P_<LWA_SAIDA_CAT02>_STATUS  text
*----------------------------------------------------------------------*
FORM check_grup_planej  USING p_equip-plangroup TYPE ty_saida_cat02-plangroup p_equip-id TYPE ty_saida_cat02-id p_equip-status TYPE ty_saida_cat02-status CHANGING txt TYPE char50 status TYPE char05.
  IF p_equip-plangroup IS NOT INITIAL.
    SELECT SINGLE * FROM t024i INTO @DATA(ws_t024i) WHERE iwerk  EQ @c_centro AND ingrp EQ @p_equip-plangroup.
    IF sy-subrc NE 0.
      txt = |Grupo de planejamento { p_equip-plangroup } não cadastrado|.
      APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
      status = '@0A@'.
      p_erro = abap_true.
*    ELSE.
*      IF p_erro IS INITIAL.
*        txt = |Grupo de planejamento validado|.
*        APPEND VALUE #( id = p_equip-id status = '@08@' type = 'S' desc_status = txt ) TO t_status.
*        status = '@08@'.
*      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_PERF_CATALAGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LWA_SAIDA_CAT02>  text
*      <--P_TXT  text
*      <--P_<LWA_SAIDA_CAT02>_STATUS  text
*----------------------------------------------------------------------*
FORM check_perf_catalago  USING  p_equip-catprofile TYPE ty_saida_cat02-catprofile  p_equip-id TYPE ty_saida_cat02-id p_equip-status TYPE ty_saida_cat02-status CHANGING txt TYPE char50 status TYPE char05.
  DATA: i_msgty TYPE sy-msgty,
        e_rbnrx TYPE t352b_t-rbnrx.

  IF p_equip-catprofile IS NOT INITIAL.
    CALL FUNCTION 'ITOB_CHECK_CATALOG_PROFILE'
      EXPORTING
        rbnr              = p_equip-catprofile
        dialog_cursor     = 'ITOB-RBNR'
        x_mess_type       = i_msgty
      IMPORTING
        rbnrx_exp         = e_rbnrx
      EXCEPTIONS
        application_error = 2
        OTHERS            = 3.

    IF sy-subrc NE 0.
      txt = |Grupo de planejamento { p_equip-catprofile } não cadastrado|.
      APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
      status = '@0A@'.
      p_erro = abap_true.
*    ELSE.
*      IF p_erro IS INITIAL.
*        txt = |Grupo de planejamento validado|.
*        APPEND VALUE #( id = p_equip-id status = '@08@' type = 'S' desc_status = txt ) TO t_status.
*        status = '@08@'.
*      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_CONJUNTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LWA_SAIDA_CAT02>  text
*      <--P_TXT  text
*      <--P_<LWA_SAIDA_CAT02>_STATUS  text
*----------------------------------------------------------------------*
FORM check_conjunto  USING  p_equip-consttype TYPE ty_saida_cat02-consttype p_equip-id TYPE ty_saida_cat02-id p_equip-status TYPE ty_saida_cat02-status CHANGING txt TYPE char50 status TYPE char05.

  IF p_equip-consttype IS NOT INITIAL.

    p_equip-consttype = |{ p_equip-consttype ALPHA  = IN }|.
    SELECT SINGLE * FROM m_mat1j INTO @DATA(ws_m_mat1j) WHERE matnr  EQ @p_equip-consttype AND spras EQ @sy-langu.
    IF sy-subrc NE 0.
      txt = |Conjunto { p_equip-consttype } não cadastrado|.
      APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
      status = '@0A@'.
      p_erro = abap_true.
*    ELSE.
*      IF p_erro IS INITIAL.
*        txt = |Conjunto validado|.
*        APPEND VALUE #( id = p_equip-id status = '@08@' type = 'S' desc_status = txt ) TO t_status.
*        status = '@08@'.
*      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_UNID_PESO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LWA_SAIDA_CAT02>  text
*      <--P_TXT  text
*      <--P_<LWA_SAIDA_CAT02>_STATUS  text
*----------------------------------------------------------------------*
FORM check_unid_peso  USING p_equip-unit_of_wt TYPE ty_saida_cat02-unit_of_wt p_equip-id TYPE ty_saida_cat02-id p_equip-status TYPE ty_saida_cat02-status CHANGING txt TYPE char50 status TYPE char05.

  IF p_equip-unit_of_wt IS NOT INITIAL.
    SELECT SINGLE * FROM t006b INTO @DATA(ws_t006b) WHERE mseh3  EQ @p_equip-unit_of_wt AND spras EQ @sy-langu.
    IF sy-subrc NE 0.
      txt = |Unidade peso { p_equip-unit_of_wt } não cadastrado|.
      APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
      status = '@0A@'.
      p_erro = abap_true.
*    ELSE.
*      IF p_erro IS INITIAL.
*        txt = |Conjunto validado|.
*        APPEND VALUE #( id = p_equip-id status = '@08@' type = 'S' desc_status = txt ) TO t_status.
*        status = '@08@'.
*      ENDIF.
    ENDIF.
  ENDIF.



ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHECK_LOCAL_INSTALACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LWA_SAIDA_CAT02>  text
*      <--P_TXT  text
*      <--P_<LWA_SAIDA_CAT02>_STATUS  text
*----------------------------------------------------------------------*
FORM check_local_instalacao USING p_equip-read_floc TYPE ty_saida_cat02-read_floc p_equip-id TYPE ty_saida_cat02-id p_equip-status TYPE ty_saida_cat02-status CHANGING txt TYPE char50 status TYPE char05.
  DATA: cont_local TYPE p.
  CLEAR: cont_local.
  IF p_equip-read_floc IS NOT INITIAL.
    SELECT SINGLE * FROM iflo INTO @DATA(ws_iflo) WHERE tplnr EQ @p_equip-read_floc.
    IF sy-subrc NE 0.
      txt = |Local instalação { p_equip-read_floc } não cadastrado|.
      APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
      status = '@0A@'.
      p_erro = abap_true.
    ELSE.
      CLEAR: ws_iflo.
      "Check se o local permite atribuição individual equipamento.

      SELECT SINGLE * FROM iflo INTO @ws_iflo WHERE tplnr EQ @p_equip-read_floc AND einzl EQ @abap_true.
      IF sy-subrc EQ 0.
        "Check se ja existe equipamento atribuido.
        SELECT * FROM iloa INTO TABLE @DATA(t_iloa) WHERE tplnr EQ @p_equip-read_floc.
        SORT t_iloa DESCENDING BY iloan.
        READ TABLE t_iloa INTO DATA(ws_iloa) INDEX 1.
        SELECT SINGLE * FROM equz INTO @DATA(ws_equz) WHERE iloan EQ @ws_iloa-iloan AND datbi EQ '99991231'.
        IF sy-subrc EQ 0.
          txt = |Ja existe equipamento atribuido para local de instalação { p_equip-read_floc  }|.
          APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          status = '@0A@'.
          p_erro = abap_true.
        ELSE.
          "check local se em mais de um equipamento na planilha.
          LOOP AT git_saida_cat02 ASSIGNING FIELD-SYMBOL(<ws_cat02>) WHERE read_floc EQ p_equip-read_floc.
            ADD 1 TO cont_local.
          ENDLOOP.

          IF cont_local > 1.
            txt = |Existe mais de 1 equipamento atribuido para local de instalação { p_equip-read_floc  }|.
            APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            status = '@0A@'.
            p_erro = abap_true.
          ENDIF.
        ENDIF.
      ELSE.

      ENDIF.
    ENDIF.
  ELSE.
    txt = |Prencher o local de instalação|.
    APPEND VALUE #( id = p_equip-id status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
    status = '@0A@'.
    p_erro = abap_true.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_MODIFICA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_modifica .


  DATA: lwa_erros   LIKE LINE OF git_erros,
        lva_linha   TYPE sy-tabix,
        lva_exit    TYPE c,
        lva_fun_loc TYPE  iflo-tplnr,
        lva_x       TYPE i,
        lva_string  TYPE string.

  DATA:
    lit_equipment_list TYPE STANDARD TABLE OF alm_me_inventory_mng_item,
    lwa_equipment_list LIKE LINE OF lit_equipment_list.

  FIELD-SYMBOLS: <lva_field> TYPE any.

  gva_verificado = ''.
  gva_erro = ''.
  CLEAR: git_erros[].

  LOOP AT  git_saida_cat02_m INTO DATA(lwa_saida_cat02_m).
    lva_linha = sy-tabix.
    CLEAR: gva_valid_date, gva_equipment.

    gva_equipment = lwa_saida_cat02_m-equipment.

    IF lwa_saida_cat02_m-valid_date IS INITIAL.
      gva_valid_date = sy-datum.
    ELSE.
      gva_valid_date = lwa_saida_cat02_m-valid_date.
    ENDIF.

    gwa_dataspecific-equicatgry            =   lwa_saida_cat02_m-equicatgry.
    gwa_datageneral-authgrp                =   lwa_saida_cat02_m-authgrp.
    gwa_datageneral-obj_weight             =   lwa_saida_cat02_m-obj_weight.
    gwa_datageneral-unit_of_wt             =   lwa_saida_cat02_m-unit_of_wt.
    gwa_datageneral-obj_size               =   lwa_saida_cat02_m-obj_size.
    gwa_datageneral-inventory              =   lwa_saida_cat02_m-inventory.
    gwa_datageneral-start_from             =   lwa_saida_cat02_m-start_from.
    gwa_datageneral-objecttype             =   lwa_saida_cat02_m-objecttype.
    gwa_datageneral-acquisval              =   lwa_saida_cat02_m-acquisval.
    gwa_datageneral-currency               =   lwa_saida_cat02_m-currency.
    gwa_datageneral-acqdate                =   lwa_saida_cat02_m-acqdate.
    gwa_datageneral-manfacture             =   lwa_saida_cat02_m-manfacture.
    gwa_datageneral-mancountry             =   lwa_saida_cat02_m-mancountry.
    gwa_datageneral-manmodel               =   lwa_saida_cat02_m-manmodel.
    gwa_datageneral-constyear              =   lwa_saida_cat02_m-constyear.
    gwa_datageneral-constmonth             =   lwa_saida_cat02_m-constmonth.
    gwa_datageneral-manparno               =   lwa_saida_cat02_m-manparno.
    gwa_datageneral-manserno               =   lwa_saida_cat02_m-manserno.
    gwa_datageneral-descript               =   lwa_saida_cat02_m-descript.
    gwa_datageneral-start_from             =   lwa_saida_cat02_m-start_from.
    gwa_datageneral-maintplant             =   lwa_saida_cat02_m-maintplant.
    gwa_datageneral-maintloc               =   lwa_saida_cat02_m-maintloc.
    gwa_datageneral-maintroom              =   lwa_saida_cat02_m-maintroom . "Teste Sala - ABAP
    gwa_datageneral-plsectn                =   lwa_saida_cat02_m-plsectn.
    gwa_datageneral-pp_wkctr               =   lwa_saida_cat02_m-pp_wkctr.
    gwa_datageneral-abcindic               =   lwa_saida_cat02_m-abcindic.
    gwa_datageneral-sortfield              =   lwa_saida_cat02_m-sortfield.
    gwa_datageneral-comp_code              =   lwa_saida_cat02_m-comp_code.
    gwa_datageneral-bus_area               =   lwa_saida_cat02_m-bus_area.
    gwa_datageneral-asset_no               =   lwa_saida_cat02_m-asset_no.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-asset_no
      IMPORTING
        output = gwa_datageneral-asset_no.

    gwa_datageneral-costcenter             =   lwa_saida_cat02_m-costcenter.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-costcenter
      IMPORTING
        output = gwa_datageneral-costcenter.

    gwa_datageneral-wbs_elem               =   lwa_saida_cat02_m-wbs_elem.

    gwa_datageneral-standorder             =   lwa_saida_cat02_m-standorder.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-standorder
      IMPORTING
        output = gwa_datageneral-standorder.


    gwa_datageneral-settlorder             =   lwa_saida_cat02_m-settlorder.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-settlorder
      IMPORTING
        output = gwa_datageneral-settlorder.

    gwa_datageneral-planplant              =   lwa_saida_cat02_m-planplant.
    gwa_datageneral-plangroup              =   lwa_saida_cat02_m-plangroup.
    gwa_datageneral-work_ctr               =   lwa_saida_cat02_m-work_ctr.
    gwa_datageneral-catprofile             =   lwa_saida_cat02_m-catprofile.
    gwa_dataspecific-read_floc             =   lwa_saida_cat02_m-read_floc.
*---> 07/07/2023 - Migração S4 - RZ
*    gwa_datageneral-consttype              =   lwa_saida_cat02_m-consttype.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = gwa_datageneral-consttype
*      IMPORTING
*        output = gwa_datageneral-consttype.

    DATA(v_len) = strlen( lwa_saida_cat02_m-consttype ).

    IF v_len > 18.
      gwa_datageneral-consttype_long  =   lwa_saida_cat02_m-consttype.
    ELSE.
      gwa_datageneral-consttype       =   lwa_saida_cat02_m-consttype.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gwa_datageneral-consttype
        IMPORTING
          output = gwa_datageneral-consttype.
    ENDIF.
*<--- 07/07/2023 - Migração S4 - RZ

    gwa_dataspecific-inst_pos     =   lwa_saida_cat02_m-inst_pos.
    gwa_dataspecific-techid       =   lwa_saida_cat02_m-techid.
    gwa_datafleet-fleet_num       =   lwa_saida_cat02_m-fleet_num.
    gwa_datafleet-license_num     =   lwa_saida_cat02_m-license_num.
    gwa_datafleet-expiry_date     =   lwa_saida_cat02_m-expiry_date.
    gwa_datafleet-fleet_vin       =   lwa_saida_cat02_m-fleet_vin.
    gwa_datafleet-chassis_num     =   lwa_saida_cat02_m-chassis_num.
    gwa_datafleet-gross_wgt       =   lwa_saida_cat02_m-gross_wgt.
    gwa_datafleet-load_wgt        =   lwa_saida_cat02_m-load_wgt.
    gwa_datafleet-load_vol        =   lwa_saida_cat02_m-load_vol.
    gwa_datafleet-vol_unit        =   lwa_saida_cat02_m-vol_unit.
    gwa_datafleet-load_hgt        =   lwa_saida_cat02_m-load_hgt.
    gwa_datafleet-load_dim_unit   =   lwa_saida_cat02_m-load_dim_unit.
    gwa_datafleet-load_wid        =   lwa_saida_cat02_m-load_wid.
    gwa_datafleet-load_len        =   lwa_saida_cat02_m-load_len.
    gwa_datafleet-no_compart      =   lwa_saida_cat02_m-no_compart.
    gwa_datafleet-fleet_hgt       =   lwa_saida_cat02_m-fleet_hgt.
    gwa_datafleet-dim_unit        =   lwa_saida_cat02_m-dim_unit.
    gwa_datafleet-fleet_wid       =   lwa_saida_cat02_m-fleet_wid.
    gwa_datafleet-fleet_len       =   lwa_saida_cat02_m-fleet_len.
    gwa_datafleet-repla_date      =   lwa_saida_cat02_m-repla_date.
    gwa_datafleet-repla_odom      =   lwa_saida_cat02_m-repla_odom.
    gwa_datafleet-repla_oph       =   lwa_saida_cat02_m-repla_oph.
    gwa_datafleet-fleet_use       =   lwa_saida_cat02_m-fleet_use.
    gwa_datafleet-card_num        =   lwa_saida_cat02_m-card_num.
    gwa_datafleet-max_occupants   =   lwa_saida_cat02_m-max_occupants.
    gwa_datafleet-key_num         =   lwa_saida_cat02_m-key_num.
    gwa_datafleet-num_axle        =   lwa_saida_cat02_m-num_axle.
    gwa_datafleet-engine_type     =   lwa_saida_cat02_m-engine_type.
    gwa_datafleet-engine_snr      =   lwa_saida_cat02_m-engine_snr.
    gwa_datafleet-speed_max       =   lwa_saida_cat02_m-speed_max.
    gwa_datafleet-speed_unit      =   lwa_saida_cat02_m-speed_unit.
    gwa_datafleet-engine_power    =   lwa_saida_cat02_m-engine_power.
    gwa_datafleet-unit_power      =   lwa_saida_cat02_m-unit_power.
    gwa_datafleet-revolutions     =   lwa_saida_cat02_m-revolutions.
    gwa_datafleet-engine_cap      =   lwa_saida_cat02_m-engine_cap.
    gwa_datafleet-unit_cap        =   lwa_saida_cat02_m-unit_cap.
    gwa_datafleet-engine_cyl      =   lwa_saida_cat02_m-engine_cyl.
    gwa_datafleet-fuel_pri        =   lwa_saida_cat02_m-fuel_pri.
    gwa_datafleet-fuel_sec        =   lwa_saida_cat02_m-fuel_sec.
    gwa_datafleet-oil_type        =   lwa_saida_cat02_m-oil_type.
    gwa_datafleet-pri_calc        =   lwa_saida_cat02_m-pri_calc.

    APPEND VALUE #( structure = 'DIV1' valuepart1 = '07116DD809592015' ) TO git_xtensionin.
    APPEND VALUE #( structure = 'DIV2' valuepart1 = '07116DD809592015' ) TO git_xtensionin.
    APPEND VALUE #( structure = 'DIV3' valuepart1 = '07116DD809592015' ) TO git_xtensionin.

    APPEND VALUE #( structure = 'TQ_COMBUSTIVEL_1' valuepart1 = '120' ) TO git_xtensionin.
    APPEND VALUE #( structure = 'TQ_COMBUSTIVEL_2' valuepart1 = '120' ) TO git_xtensionin.
    APPEND VALUE #( structure = 'TQ_COMBUSTIVEL_3' valuepart1 = '120' ) TO git_xtensionin.

    "datafleet-div1               =   lwa_saida_cat02_m-div1.
    "datafleet-tq_combustivel_1   =   lwa_saida_cat02_m-tq_combustivel_1.
    "datafleet-div2               =   lwa_saida_cat02_m-div2 .
    "datafleet-tq_combustivel_2   =   lwa_saida_cat02_m-tq_combustivel_2.
    "datafleet-div3               =   lwa_saida_cat02_m-div3.
    "datafleet-tq_combustivel_3   =   lwa_saida_cat02_m-tq_combustivel_3.


    gwa_dataspecific-inst_pos    =   lwa_saida_cat02_m-inst_pos.
    gwa_dataspecific-techid      =   lwa_saida_cat02_m-techid.

    IF gwa_datageneral-standorder IS NOT INITIAL.
      gwa_data_generalx-standorder = 'X'.
    ENDIF.

    IF gwa_datageneral-settlorder IS NOT INITIAL.
      gwa_data_generalx-settlorder = 'X'.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gva_equipment
      IMPORTING
        output = gva_equipment.

    DO 92 TIMES.
      "Tratativa para campos de material realizada. Pseudo comentário adicionado   " >> ---> S4 Migration - 07/07/2023 - RZ
      CALL FUNCTION 'BAPI_EQUI_CHANGE'                 "#EC CI_USAGE_OK[2438131]   " >> ---> S4 Migration - 07/07/2023 - RZ
        EXPORTING
          equipment      = gva_equipment
          data_general   = gwa_datageneral
          data_generalx  = gwa_data_generalx
          data_specific  = gwa_dataspecific
          data_specificx = gwa_data_specificx
          valid_date     = sy-datum
          valid_time     = sy-uzeit
        IMPORTING
          return         = gwa_return
        TABLES
          extensionin    = git_xtensionin.

      IF gwa_return IS NOT INITIAL.
        DO.
          ASSIGN COMPONENT sy-index OF STRUCTURE gwa_datageneral TO <lva_field>.
          IF sy-subrc IS INITIAL.
            MOVE <lva_field> TO lva_string.
            FIND ALL OCCURRENCES OF gwa_return-message_v1 IN lva_string  MATCH COUNT lva_x.
            IF sy-subrc = 0.
              <lva_field> = ''.
            ENDIF.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.

        lwa_erros-linha      = lva_linha.
        lwa_erros-id         = gwa_return-id(2).
        lwa_erros-type       = gwa_return-type.
        lwa_erros-number     = gwa_return-number.
        lwa_erros-message    = gwa_return-message.
        lwa_erros-message_v1 = gwa_return-message_v1.
        lwa_erros-message_v2 = gwa_return-message_v2.
        lwa_erros-message_v3 = gwa_return-message_v3.

        APPEND  lwa_erros TO git_erros.
        CLEAR: lwa_erros.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_MODIFICA_EQPTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_modifica_eqpto .

  DATA: lwa_erros   LIKE LINE OF git_erros,
        lva_linha   TYPE sy-tabix,
        lva_exit    TYPE c,
        lva_fun_loc TYPE  iflo-tplnr,
        lva_x       TYPE i,
        lva_string  TYPE string.

  DATA:
    lit_equipment_list TYPE STANDARD TABLE OF alm_me_inventory_mng_item,
    lwa_equipment_list LIKE LINE OF lit_equipment_list.

  FIELD-SYMBOLS: <lva_field> TYPE any.

  LOOP AT  git_saida_cat01_m INTO DATA(lwa_saida_cat01_m).
    lva_linha = sy-tabix.
    CLEAR: gva_valid_date, gva_equipment.

    gva_equipment = lwa_saida_cat01_m-equipment.

    IF lwa_saida_cat01_m-valid_date IS INITIAL.
      gva_valid_date = sy-datum.
    ELSE.
      gva_valid_date = lwa_saida_cat01_m-valid_date.
    ENDIF.

    gwa_datageneral-authgrp      =   lwa_saida_cat01_m-authgrp   .
    gwa_datageneral-obj_weight   =   lwa_saida_cat01_m-obj_weight .
    gwa_datageneral-unit_of_wt   =   lwa_saida_cat01_m-unit_of_wt .
    gwa_datageneral-obj_size     =   lwa_saida_cat01_m-obj_size  .
    gwa_datageneral-inventory    =   lwa_saida_cat01_m-inventory .
    gwa_datageneral-start_from   =   lwa_saida_cat01_m-start_from .
    gwa_datageneral-objecttype   =   lwa_saida_cat01_m-objecttype .
    gwa_datageneral-acquisval    =   lwa_saida_cat01_m-acquisval  .
    gwa_datageneral-currency     =   lwa_saida_cat01_m-currency .
    gwa_datageneral-acqdate      =   lwa_saida_cat01_m-acqdate   .
    gwa_datageneral-manfacture   =   lwa_saida_cat01_m-manfacture .
    gwa_datageneral-mancountry   =   lwa_saida_cat01_m-mancountry .
    gwa_datageneral-manmodel     =   lwa_saida_cat01_m-manmodel .
    gwa_datageneral-constyear    =   lwa_saida_cat01_m-constyear  .
    gwa_datageneral-constmonth   =   lwa_saida_cat01_m-constmonth .
    gwa_datageneral-manparno     =   lwa_saida_cat01_m-manparno  .
    gwa_datageneral-manserno     =   lwa_saida_cat01_m-manserno .
    gwa_datageneral-descript     =   lwa_saida_cat01_m-descript  .
    gwa_datageneral-start_from   =   lwa_saida_cat01_m-start_from .
    gwa_datageneral-maintplant   =   lwa_saida_cat01_m-maintplant.
    gwa_datageneral-maintloc     =   lwa_saida_cat01_m-maintloc .
    gwa_datageneral-maintroom    =   lwa_saida_cat01_m-maintroom . "Teste Sala - ABAP
    gwa_datageneral-plsectn      =   lwa_saida_cat01_m-plsectn .
    gwa_datageneral-pp_wkctr     =   lwa_saida_cat01_m-pp_wkctr .  "Verificar ABAP
    gwa_datageneral-abcindic     =   lwa_saida_cat01_m-abcindic.
    gwa_datageneral-sortfield    =   lwa_saida_cat01_m-sortfield  .
    gwa_datageneral-comp_code    =   lwa_saida_cat01_m-comp_code .
    gwa_datageneral-bus_area     =   lwa_saida_cat01_m-bus_area .
    gwa_datageneral-asset_no     =   lwa_saida_cat01_m-asset_no .

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-asset_no
      IMPORTING
        output = gwa_datageneral-asset_no.

    gwa_datageneral-planplant    =   lwa_saida_cat01_m-planplant.
    gwa_datageneral-sub_number   =   lwa_saida_cat01_m-sub_number .

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-sub_number
      IMPORTING
        output = gwa_datageneral-sub_number.

    gwa_datageneral-costcenter   =   lwa_saida_cat01_m-costcenter.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-costcenter
      IMPORTING
        output = gwa_datageneral-costcenter.

    gwa_datageneral-wbs_elem     =   lwa_saida_cat01_m-wbs_elem .
    gwa_datageneral-standorder   =   lwa_saida_cat01_m-standorder .

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-standorder
      IMPORTING
        output = gwa_datageneral-standorder.

    gwa_datageneral-settlorder   =   lwa_saida_cat01_m-settlorder.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-settlorder
      IMPORTING
        output = gwa_datageneral-settlorder.

    gwa_datageneral-plangroup    =   lwa_saida_cat01_m-plangroup.
    gwa_datageneral-work_ctr     =   lwa_saida_cat01_m-work_ctr.
    gwa_datageneral-catprofile   =   lwa_saida_cat01_m-catprofile .
    gwa_datageneral-pp_wkctr     =   lwa_saida_cat01_m-pp_wkctr.
    gwa_datageneral-consttype    =   lwa_saida_cat01_m-consttype.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-consttype
      IMPORTING
        output = gwa_datageneral-consttype.

    gwa_datainstall-funcloc      =   lwa_saida_cat01_m-read_floc.
    gwa_datainstall-inst_pos     =   lwa_saida_cat01_m-inst_pos.

    gwa_dataspecific-inst_pos    =   lwa_saida_cat01_m-inst_pos.
    gwa_dataspecific-techid      =   lwa_saida_cat01_m-techid.
    gwa_dataspecific-equicatgry  =   lwa_saida_cat01_m-equicatgry.
    gwa_dataspecific-read_floc   =   lwa_saida_cat01_m-read_floc.

    IF gwa_datageneral-standorder IS NOT INITIAL.
      gwa_data_generalx-standorder = 'X'.
    ENDIF.

    IF gwa_datageneral-settlorder IS NOT INITIAL.
      gwa_data_generalx-settlorder = 'X'.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gva_equipment
      IMPORTING
        output = gva_equipment.

    DO 47 TIMES.
      CALL FUNCTION 'BAPI_EQUI_CHANGE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          equipment      = gva_equipment
          data_general   = gwa_datageneral
          data_generalx  = gwa_data_generalx
          data_specific  = gwa_dataspecific
          data_specificx = gwa_data_specificx
          valid_date     = sy-datum
          valid_time     = sy-uzeit
        IMPORTING
          return         = gwa_return.

      IF gwa_return IS NOT INITIAL.
        DO.
          ASSIGN COMPONENT sy-index OF STRUCTURE gwa_datageneral TO <lva_field>.
          IF sy-subrc IS INITIAL.
            MOVE <lva_field> TO lva_string.
            FIND ALL OCCURRENCES OF gwa_return-message_v1 IN lva_string  MATCH COUNT lva_x.
            IF sy-subrc = 0.
              <lva_field> = ''.
            ENDIF.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.

        lwa_erros-linha      = lva_linha.
        lwa_erros-id         = gwa_return-id(2).
        lwa_erros-type       = gwa_return-type.
        lwa_erros-number     = gwa_return-number.
        lwa_erros-message    = gwa_return-message.
        lwa_erros-message_v1 = gwa_return-message_v1.
        lwa_erros-message_v2 = gwa_return-message_v2.
        lwa_erros-message_v3 = gwa_return-message_v3.

        APPEND  lwa_erros TO git_erros.
        CLEAR: lwa_erros.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIAR_EQPTO_DIV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_criar_eqpto_div .
  DATA: lva_equipment LIKE bapi_itob_parms-equipment,
        lva_wait      LIKE bapita-wait,
        lva_return    TYPE bapireturn,
        lva_linha     TYPE sy-tabix,
        lwa_erros     LIKE LINE OF git_erros.

  CLEAR: gva_valid_date, gva_equipment, gwa_return, gwa_datageneral, gwa_datafleet, gwa_dataspecific.

  LOOP AT  git_saida_cat01 ASSIGNING FIELD-SYMBOL(<lwa_saida_cat01>).
    lva_linha = sy-tabix.
    CLEAR: gva_valid_date, local.

    IF <lwa_saida_cat01>-valid_date IS INITIAL.
      gva_valid_date = sy-datum.
    ELSE.
      gva_valid_date = <lwa_saida_cat01>-valid_date.
    ENDIF.

    gwa_dataspecific-equicatgry  =   <lwa_saida_cat01>-equicatgry.
    gwa_datageneral-authgrp      =   <lwa_saida_cat01>-authgrp   .
    gwa_datageneral-obj_weight   =   <lwa_saida_cat01>-obj_weight .
    gwa_datageneral-unit_of_wt   =   <lwa_saida_cat01>-unit_of_wt .
    gwa_datageneral-obj_size     =   <lwa_saida_cat01>-obj_size  .
    gwa_datageneral-inventory    =   <lwa_saida_cat01>-inventory .
    gwa_datageneral-start_from   =   <lwa_saida_cat01>-start_from .
    gwa_datageneral-objecttype   =   <lwa_saida_cat01>-objecttype .
    gwa_datageneral-acquisval    =   <lwa_saida_cat01>-acquisval  .
    gwa_datageneral-currency     =   <lwa_saida_cat01>-currency .
    gwa_datageneral-acqdate      =   <lwa_saida_cat01>-acqdate   .
    gwa_datageneral-manfacture   =   <lwa_saida_cat01>-manfacture .
    gwa_datageneral-mancountry   =   <lwa_saida_cat01>-mancountry .
    gwa_datageneral-manmodel     =   <lwa_saida_cat01>-manmodel .
    gwa_datageneral-constyear    =   <lwa_saida_cat01>-constyear  .
    gwa_datageneral-constmonth   =   <lwa_saida_cat01>-constmonth .
    gwa_datageneral-manparno     =   <lwa_saida_cat01>-manparno  .
    gwa_datageneral-manserno     =   <lwa_saida_cat01>-manserno .
    gwa_datageneral-descript     =   <lwa_saida_cat01>-descript  .
    gwa_datageneral-start_from   =   <lwa_saida_cat01>-start_from .
    gwa_datageneral-maintplant   =   <lwa_saida_cat01>-maintplant.
    gwa_datageneral-maintloc     =   <lwa_saida_cat01>-maintloc .
    gwa_datageneral-maintroom    =   <lwa_saida_cat01>-maintroom . "Teste Sala - ABAP
    gwa_datageneral-plsectn      =   <lwa_saida_cat01>-plsectn .
    gwa_datageneral-pp_wkctr     =   <lwa_saida_cat01>-pp_wkctr .  "Verificar ABAP
    gwa_datageneral-abcindic     =   <lwa_saida_cat01>-abcindic.
    gwa_datageneral-sortfield    =   <lwa_saida_cat01>-sortfield  .
    gwa_datageneral-comp_code    =   <lwa_saida_cat01>-comp_code .
    gwa_datageneral-bus_area     =   <lwa_saida_cat01>-bus_area .
    gwa_datageneral-asset_no     =   <lwa_saida_cat01>-asset_no .

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-asset_no
      IMPORTING
        output = gwa_datageneral-asset_no.

    gwa_datageneral-planplant    =   <lwa_saida_cat01>-planplant.
    gwa_datageneral-sub_number   =   <lwa_saida_cat01>-sub_number .

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-sub_number
      IMPORTING
        output = gwa_datageneral-sub_number.

    gwa_datageneral-costcenter   =   <lwa_saida_cat01>-costcenter.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-costcenter
      IMPORTING
        output = gwa_datageneral-costcenter.

    gwa_datageneral-wbs_elem     =   <lwa_saida_cat01>-wbs_elem .
    gwa_datageneral-standorder   =   <lwa_saida_cat01>-standorder .

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-standorder
      IMPORTING
        output = gwa_datageneral-standorder.

    gwa_datageneral-settlorder   =   <lwa_saida_cat01>-settlorder.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-settlorder
      IMPORTING
        output = gwa_datageneral-settlorder.

    gwa_datageneral-plangroup    =   <lwa_saida_cat01>-plangroup.
    gwa_datageneral-work_ctr     =   <lwa_saida_cat01>-work_ctr.
    gwa_datageneral-catprofile   =   <lwa_saida_cat01>-catprofile .
    gwa_datageneral-pp_wkctr     =   <lwa_saida_cat01>-pp_wkctr.
    gwa_dataspecific-read_floc   =   <lwa_saida_cat01>-read_floc.
    gwa_datageneral-consttype    =   <lwa_saida_cat01>-consttype.
    gwa_datainstall-funcloc      =   <lwa_saida_cat01>-read_floc.
    gwa_datainstall-inst_pos     =   <lwa_saida_cat01>-inst_pos.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-consttype
      IMPORTING
        output = gwa_datageneral-consttype.

    gwa_dataspecific-inst_pos    =   <lwa_saida_cat01>-inst_pos.
    gwa_dataspecific-techid      =   <lwa_saida_cat01>-techid.

    CALL FUNCTION 'BAPI_EQUI_CREATE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        data_general      = gwa_datageneral
        data_specific     = gwa_dataspecific
        data_fleet        = gwa_datafleet
        valid_date        = gva_valid_date
        data_install      = gwa_datainstall
      IMPORTING
        equipment         = lva_equipment
        data_general_exp  = gwa_datageneralexp
        data_specific_exp = gwa_dataspecificexp
        data_fleet_exp    = gwa_datafleetexp
        return            = gwa_return
      EXCEPTIONS
        OTHERS            = 01.
    CASE sy-subrc.
      WHEN 0.            " OK
      WHEN OTHERS.       " to be implemented
    ENDCASE.

    IF lva_equipment IS NOT INITIAL.
      lva_wait = 'X' .
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = lva_wait.
      <lwa_saida_cat01>-external_number = lva_equipment.
      <lwa_saida_cat01>-status = '@08@'.
      DATA: eqpto TYPE bapi_equi-equipment.
      local = CONV #( gwa_dataspecific-read_floc ).
      PERFORM fm_monta_eqpto_local USING lva_equipment local.
    ELSE.
      <lwa_saida_cat01>-status = '@0A@'.
      APPEND VALUE #( id = <lwa_saida_cat01>-id status = '@0A@' type = 'E' desc_status = gwa_return-message ) TO t_status.

*      lwa_erros-linha      = lva_linha.
*      lwa_erros-id         = gwa_return-id(2).
*      lwa_erros-type       = gwa_return-type.
*      lwa_erros-number     = gwa_return-number.
*      lwa_erros-message    = gwa_return-message.
*      lwa_erros-message_v1 = gwa_return-message_v1.
*      lwa_erros-message_v2 = gwa_return-message_v2.
*      lwa_erros-message_v3 = gwa_return-message_v3.
*
*      APPEND  lwa_erros TO git_erros.
      CLEAR: lwa_erros.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*      EXIT.
    ENDIF.
*    PERFORM fm_log_erros.
    CLEAR: gwa_dataspecific.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIAR_EQPTO_FROTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_criar_eqpto_frota .
  DATA: lva_equipment LIKE bapi_itob_parms-equipment,
        lva_wait      LIKE bapita-wait,
        lva_return    TYPE bapireturn,
        lva_linha     TYPE sy-tabix,
        lwa_erros     LIKE LINE OF git_erros.

  CLEAR:  git_erros, gwa_datageneral.

  LOOP AT  git_saida_cat02 ASSIGNING FIELD-SYMBOL(<lwa_saida_cat02>).
    lva_linha = sy-tabix.
    CLEAR: gva_valid_date, gva_equipment, gwa_return, gwa_datageneral, gwa_datafleet, gwa_dataspecific.

    gva_external_number = |{ <lwa_saida_cat02>-external_number ALPHA = IN }|.

    IF <lwa_saida_cat02>-valid_date IS INITIAL.
      gva_valid_date = sy-datum.
    ELSE.
      gva_valid_date = <lwa_saida_cat02>-valid_date.
    ENDIF.

    gwa_dataspecific-equicatgry            =   <lwa_saida_cat02>-equicatgry.
    gwa_datageneral-authgrp                =   <lwa_saida_cat02>-authgrp.
    gwa_datageneral-obj_weight             =   <lwa_saida_cat02>-obj_weight.
    gwa_datageneral-unit_of_wt             =   <lwa_saida_cat02>-unit_of_wt.
    gwa_datageneral-obj_size               =   <lwa_saida_cat02>-obj_size.
    gwa_datageneral-inventory              =   <lwa_saida_cat02>-inventory.
    gwa_datageneral-start_from             =   <lwa_saida_cat02>-start_from.
    gwa_datageneral-objecttype             =   <lwa_saida_cat02>-objecttype.
    gwa_datageneral-acquisval              =   <lwa_saida_cat02>-acquisval.
    gwa_datageneral-currency               =   <lwa_saida_cat02>-currency.
    gwa_datageneral-acqdate                =   <lwa_saida_cat02>-acqdate.
    gwa_datageneral-manfacture             =   <lwa_saida_cat02>-manfacture.
    gwa_datageneral-mancountry             =   <lwa_saida_cat02>-mancountry.
    gwa_datageneral-manmodel               =   <lwa_saida_cat02>-manmodel.
    gwa_datageneral-constyear              =   <lwa_saida_cat02>-constyear.
    gwa_datageneral-constmonth             =   <lwa_saida_cat02>-constmonth.
    gwa_datageneral-manparno               =   <lwa_saida_cat02>-manparno.
    gwa_datageneral-manserno               =   <lwa_saida_cat02>-manserno.
    gwa_datageneral-descript               =   <lwa_saida_cat02>-descript.
    gwa_datageneral-start_from             =   <lwa_saida_cat02>-start_from.
    gwa_datageneral-maintplant             =   <lwa_saida_cat02>-maintplant.
    gwa_datageneral-maintloc               =   <lwa_saida_cat02>-maintloc.
    gwa_datageneral-maintroom              =   <lwa_saida_cat02>-maintroom . "Teste Sala - ABAP
    gwa_datageneral-plsectn                =   <lwa_saida_cat02>-plsectn.
    gwa_datageneral-pp_wkctr               =   <lwa_saida_cat02>-pp_wkctr.
    gwa_datageneral-abcindic               =   <lwa_saida_cat02>-abcindic.
    gwa_datageneral-sortfield              =   <lwa_saida_cat02>-sortfield.
    gwa_datageneral-comp_code              =   <lwa_saida_cat02>-comp_code.
    gwa_datageneral-bus_area               =   <lwa_saida_cat02>-bus_area.
    gwa_datageneral-asset_no               =   <lwa_saida_cat02>-asset_no.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-asset_no
      IMPORTING
        output = gwa_datageneral-asset_no.

    gwa_datageneral-costcenter             =   <lwa_saida_cat02>-costcenter.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-costcenter
      IMPORTING
        output = gwa_datageneral-costcenter.

    gwa_datageneral-wbs_elem               =   <lwa_saida_cat02>-wbs_elem.

    gwa_datageneral-standorder             =   <lwa_saida_cat02>-standorder.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-standorder
      IMPORTING
        output = gwa_datageneral-standorder.


    gwa_datageneral-settlorder             =   <lwa_saida_cat02>-settlorder.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-settlorder
      IMPORTING
        output = gwa_datageneral-settlorder.

    gwa_datageneral-planplant              =   <lwa_saida_cat02>-planplant.
    gwa_datageneral-plangroup              =   <lwa_saida_cat02>-plangroup.
    gwa_datageneral-work_ctr               =   <lwa_saida_cat02>-work_ctr.
    gwa_datageneral-catprofile             =   <lwa_saida_cat02>-catprofile.
    gwa_dataspecific-read_floc             =   <lwa_saida_cat02>-read_floc.
    gwa_datageneral-consttype              =   <lwa_saida_cat02>-consttype.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-consttype
      IMPORTING
        output = gwa_datageneral-consttype.

    gwa_dataspecific-inst_pos     =   <lwa_saida_cat02>-inst_pos.
    gwa_dataspecific-techid       =   <lwa_saida_cat02>-techid.
    gwa_datafleet-fleet_num       =   <lwa_saida_cat02>-fleet_num.
    gwa_datafleet-license_num     =   <lwa_saida_cat02>-license_num.
    gwa_datafleet-expiry_date     =   <lwa_saida_cat02>-expiry_date.
    gwa_datafleet-fleet_vin       =   <lwa_saida_cat02>-fleet_vin.
    gwa_datafleet-chassis_num     =   <lwa_saida_cat02>-chassis_num.
    gwa_datafleet-gross_wgt       =   <lwa_saida_cat02>-gross_wgt.
    gwa_datafleet-load_wgt        =   <lwa_saida_cat02>-load_wgt.
    gwa_datafleet-load_vol        =   <lwa_saida_cat02>-load_vol.
    gwa_datafleet-vol_unit        =   <lwa_saida_cat02>-vol_unit.
    gwa_datafleet-load_hgt        =   <lwa_saida_cat02>-load_hgt.
    gwa_datafleet-load_dim_unit   =   <lwa_saida_cat02>-load_dim_unit.
    gwa_datafleet-load_wid        =   <lwa_saida_cat02>-load_wid.
    gwa_datafleet-load_len        =   <lwa_saida_cat02>-load_len.
    gwa_datafleet-no_compart      =   <lwa_saida_cat02>-no_compart.
    gwa_datafleet-fleet_hgt       =   <lwa_saida_cat02>-fleet_hgt.
    gwa_datafleet-dim_unit        =   <lwa_saida_cat02>-dim_unit.
    gwa_datafleet-fleet_wid       =   <lwa_saida_cat02>-fleet_wid.
    gwa_datafleet-fleet_len       =   <lwa_saida_cat02>-fleet_len.
    gwa_datafleet-repla_date      =   <lwa_saida_cat02>-repla_date.
    gwa_datafleet-repla_odom      =   <lwa_saida_cat02>-repla_odom.
    gwa_datafleet-repla_oph       =   <lwa_saida_cat02>-repla_oph.
    gwa_datafleet-fleet_use       =   <lwa_saida_cat02>-fleet_use.
    gwa_datafleet-card_num        =   <lwa_saida_cat02>-card_num.
    gwa_datafleet-max_occupants   =   <lwa_saida_cat02>-max_occupants.
    gwa_datafleet-key_num         =   <lwa_saida_cat02>-key_num.
    gwa_datafleet-num_axle        =   <lwa_saida_cat02>-num_axle.
    gwa_datafleet-engine_type     =   <lwa_saida_cat02>-engine_type.
    gwa_datafleet-engine_snr      =   <lwa_saida_cat02>-engine_snr.
    gwa_datafleet-speed_max       =   <lwa_saida_cat02>-speed_max.
    gwa_datafleet-speed_unit      =   <lwa_saida_cat02>-speed_unit.
    gwa_datafleet-engine_power    =   <lwa_saida_cat02>-engine_power.
    gwa_datafleet-unit_power      =   <lwa_saida_cat02>-unit_power.
    gwa_datafleet-revolutions     =   <lwa_saida_cat02>-revolutions.
    gwa_datafleet-engine_cap      =   <lwa_saida_cat02>-engine_cap.
    gwa_datafleet-unit_cap        =   <lwa_saida_cat02>-unit_cap.
    gwa_datafleet-engine_cyl      =   <lwa_saida_cat02>-engine_cyl.
    gwa_datafleet-fuel_pri        =   <lwa_saida_cat02>-fuel_pri.
    gwa_datafleet-fuel_sec        =   <lwa_saida_cat02>-fuel_sec.
    gwa_datafleet-oil_type        =   <lwa_saida_cat02>-oil_type.
    gwa_datafleet-pri_calc        =   <lwa_saida_cat02>-pri_calc.
    "datafleet-div1               =   lwa_saida_cat02-div1.
    "datafleet-tq_combustivel_1   =   lwa_saida_cat02-tq_combustivel_1.
    "datafleet-div2               =   lwa_saida_cat02-div2 .
    "datafleet-tq_combustivel_2   =   lwa_saida_cat02-tq_combustivel_2.
    "datafleet-div3               =   lwa_saida_cat02-div3.
    "datafleet-tq_combustivel_3   =   lwa_saida_cat02-tq_combustivel_3.

    APPEND VALUE #( structure = 'DIV1' valuepart1 = '07116DD809592015' ) TO git_xtensionin.
    APPEND VALUE #( structure = 'DIV2' valuepart1 = '07116DD809592015' ) TO git_xtensionin.
    APPEND VALUE #( structure = 'DIV3' valuepart1 = '07116DD809592015' ) TO git_xtensionin.
    APPEND VALUE #( structure = 'TQ_COMBUSTIVEL_1' valuepart1 = '120' )  TO git_xtensionin.
    APPEND VALUE #( structure = 'TQ_COMBUSTIVEL_2' valuepart1 = '120' )  TO git_xtensionin.
    APPEND VALUE #( structure = 'TQ_COMBUSTIVEL_3' valuepart1 = '120' )  TO git_xtensionin.


*          DATA: tl_bapiparex TYPE TABLE OF bapiparex,
*                sl_bapiparex TYPE bapiparex,
*                lwa_ci_fleet TYPE ci_fleet.
*
*
*
*          lwa_ci_fleet-div1 = '07116DD'.
*          lwa_ci_fleet-div2 = '07116DD80959'.
*          "lwa_ci_fleet-div3 = '07116DD809592015'.
*          lwa_ci_fleet-tq_combustivel_1 = '120'.
*          lwa_ci_fleet-tq_combustivel_2 = '140'.
*          "lwa_ci_fleet-tq_combustivel_3 = '150'.
*
*          sl_bapiparex-structure       = 'CI_FLEET'.
*          sl_bapiparex-valuepart1      = lwa_ci_fleet.
*          APPEND sl_bapiparex         TO tl_bapiparex.


    gwa_dataspecific-inst_pos    =   <lwa_saida_cat02>-inst_pos.
    gwa_dataspecific-techid      =   <lwa_saida_cat02>-techid.



    IF p_equipc = 'X'.
      CALL FUNCTION 'BAPI_EQUI_CREATE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          external_number   = gva_external_number
          data_general      = gwa_datageneral
          data_specific     = gwa_dataspecific
          data_fleet        = gwa_datafleet
          valid_date        = gva_valid_date
          data_install      = gwa_datainstall
        IMPORTING
          equipment         = lva_equipment
          data_general_exp  = gwa_datageneralexp
          data_specific_exp = gwa_dataspecificexp
          data_fleet_exp    = gwa_datafleetexp
          return            = gwa_return
        TABLES
          extensionin       = git_xtensionin
        EXCEPTIONS
          OTHERS            = 01.
      CASE sy-subrc.
        WHEN 0.            " OK
        WHEN OTHERS.       " to be implemented
      ENDCASE.

      IF lva_equipment IS NOT INITIAL.
        lva_wait = 'X' .
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = lva_wait.

        <lwa_saida_cat02>-status = '@08@'.
        <lwa_saida_cat02>-external_number = lva_equipment.

        local = CONV #( gwa_dataspecific-read_floc ).
        PERFORM fm_monta_eqpto_local USING lva_equipment local. "Faz a montagem do equipamento no local de instalação.
      ELSE.
        <lwa_saida_cat02>-status = '@0A@'.
        APPEND VALUE #( id = <lwa_saida_cat02>-id status = '@0A@' type = 'E' desc_status = gwa_return-message ) TO t_status.

*        lwa_erros-linha      = lva_linha.
*        lwa_erros-id         = gwa_return-id(2).
*        lwa_erros-type       = gwa_return-type.
*        lwa_erros-number     = gwa_return-number.
*        lwa_erros-message    = gwa_return-message.
*        lwa_erros-message_v1 = gwa_return-message_v1.
*        lwa_erros-message_v2 = gwa_return-message_v2.
*        lwa_erros-message_v3 = gwa_return-message_v3.
*
*        APPEND  lwa_erros TO git_erros.
*        CLEAR: lwa_erros.


        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*        EXIT.
      ENDIF.
    ENDIF.
    CLEAR: gwa_datageneral.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_MODIFICA_EQPTO_DIV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_modifica_eqpto_div .

  DATA: lva_equipment LIKE bapi_itob_parms-equipment,
        lva_wait      LIKE bapita-wait,
        lva_return    TYPE bapireturn,
        lva_linha     TYPE sy-tabix,
        lwa_erros     LIKE LINE OF git_erros.

  CLEAR:  git_erros, gwa_datageneral.

  LOOP AT  git_saida_cat01_m ASSIGNING FIELD-SYMBOL(<lwa_saida_cat01_m>).
    lva_linha = sy-tabix.
    CLEAR: gva_valid_date, gva_equipment, gwa_return, gwa_datageneral, gwa_datafleet, gwa_dataspecific.

    gva_equipment = <lwa_saida_cat01_m>-equipment.

    IF <lwa_saida_cat01_m>-valid_date IS INITIAL.
      gva_valid_date = sy-datum.
    ELSE.
      gva_valid_date = <lwa_saida_cat01_m>-valid_date.
    ENDIF.

    gwa_datageneral-authgrp      =   <lwa_saida_cat01_m>-authgrp   .
    gwa_datageneral-obj_weight   =   <lwa_saida_cat01_m>-obj_weight .
    gwa_datageneral-unit_of_wt   =   <lwa_saida_cat01_m>-unit_of_wt .
    gwa_datageneral-obj_size     =   <lwa_saida_cat01_m>-obj_size  .
    gwa_datageneral-inventory    =   <lwa_saida_cat01_m>-inventory .
    gwa_datageneral-start_from   =   <lwa_saida_cat01_m>-start_from .
    gwa_datageneral-objecttype   =   <lwa_saida_cat01_m>-objecttype .
    gwa_datageneral-acquisval    =   <lwa_saida_cat01_m>-acquisval  .
    gwa_datageneral-currency     =   <lwa_saida_cat01_m>-currency .
    gwa_datageneral-acqdate      =   <lwa_saida_cat01_m>-acqdate   .
    gwa_datageneral-manfacture   =   <lwa_saida_cat01_m>-manfacture .
    gwa_datageneral-mancountry   =   <lwa_saida_cat01_m>-mancountry .
    gwa_datageneral-manmodel     =   <lwa_saida_cat01_m>-manmodel .
    gwa_datageneral-constyear    =   <lwa_saida_cat01_m>-constyear  .
    gwa_datageneral-constmonth   =   <lwa_saida_cat01_m>-constmonth .
    gwa_datageneral-manparno     =   <lwa_saida_cat01_m>-manparno  .
    gwa_datageneral-manserno     =   <lwa_saida_cat01_m>-manserno .
    gwa_datageneral-descript     =   <lwa_saida_cat01_m>-descript  .
    gwa_datageneral-start_from   =   <lwa_saida_cat01_m>-start_from .
    gwa_datageneral-maintplant   =   <lwa_saida_cat01_m>-maintplant.
    gwa_datageneral-maintloc     =   <lwa_saida_cat01_m>-maintloc .
    gwa_datageneral-maintroom    =   <lwa_saida_cat01_m>-maintroom .
    gwa_datageneral-plsectn      =   <lwa_saida_cat01_m>-plsectn .
    gwa_datageneral-pp_wkctr     =   <lwa_saida_cat01_m>-pp_wkctr .
    gwa_datageneral-abcindic     =   <lwa_saida_cat01_m>-abcindic.
    gwa_datageneral-sortfield    =   <lwa_saida_cat01_m>-sortfield.
    gwa_datageneral-comp_code    =   <lwa_saida_cat01_m>-comp_code.
    gwa_datageneral-bus_area     =   <lwa_saida_cat01_m>-bus_area.
    gwa_datageneral-asset_no     =   <lwa_saida_cat01_m>-asset_no.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-asset_no
      IMPORTING
        output = gwa_datageneral-asset_no.

    gwa_datageneral-planplant    =   <lwa_saida_cat01_m>-planplant.
    gwa_datageneral-sub_number   =   <lwa_saida_cat01_m>-sub_number .

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-sub_number
      IMPORTING
        output = gwa_datageneral-sub_number.

    gwa_datageneral-costcenter   =   <lwa_saida_cat01_m>-costcenter.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-costcenter
      IMPORTING
        output = gwa_datageneral-costcenter.

    gwa_datageneral-wbs_elem     =   <lwa_saida_cat01_m>-wbs_elem .
    gwa_datageneral-standorder   =   <lwa_saida_cat01_m>-standorder .

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-standorder
      IMPORTING
        output = gwa_datageneral-standorder.

    gwa_datageneral-settlorder   =   <lwa_saida_cat01_m>-settlorder.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-settlorder
      IMPORTING
        output = gwa_datageneral-settlorder.

    gwa_datageneral-plangroup    =   <lwa_saida_cat01_m>-plangroup.
    gwa_datageneral-work_ctr     =   <lwa_saida_cat01_m>-work_ctr.
    gwa_datageneral-catprofile   =   <lwa_saida_cat01_m>-catprofile .
    gwa_datageneral-pp_wkctr     =   <lwa_saida_cat01_m>-pp_wkctr.
    gwa_datageneral-consttype    =   <lwa_saida_cat01_m>-consttype.
    gwa_datainstall-funcloc      =   <lwa_saida_cat01_m>-read_floc.
    gwa_datainstall-inst_pos     =   <lwa_saida_cat01_m>-inst_pos.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-consttype
      IMPORTING
        output = gwa_datageneral-consttype.

    gwa_dataspecific-inst_pos    =   <lwa_saida_cat01_m>-inst_pos.
    gwa_dataspecific-techid      =   <lwa_saida_cat01_m>-techid.
    gwa_dataspecific-equicatgry  =   <lwa_saida_cat01_m>-equicatgry.
    gwa_dataspecific-read_floc   =   <lwa_saida_cat01_m>-read_floc.


    IF gwa_datageneral-standorder IS NOT INITIAL.
      gwa_data_generalx-standorder = 'X'.
    ENDIF.

    IF gwa_datageneral-settlorder IS NOT INITIAL.
      gwa_data_generalx-settlorder = 'X'.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gva_equipment
      IMPORTING
        output = gva_equipment.

    CALL FUNCTION 'BAPI_EQUI_CHANGE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        equipment      = gva_equipment
        data_general   = gwa_datageneral
        data_generalx  = gwa_data_generalx
        data_specific  = gwa_dataspecific
        data_specificx = gwa_data_specificx
        valid_date     = sy-datum
        valid_time     = sy-uzeit
      IMPORTING
        return         = gwa_return.

    IF gwa_return IS INITIAL.
      lva_wait = 'X' .
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = lva_wait.

      <lwa_saida_cat01_m>-status = '@08@'.
    ELSE.
      <lwa_saida_cat01_m>-status = '@0A@'.
      APPEND VALUE #( id = <lwa_saida_cat01_m>-id status = '@0A@' type = 'E' desc_status = gwa_return-message ) TO t_status.

*      lwa_erros-linha      = lva_linha.
*      lwa_erros-id         = gwa_return-id(2).
*      lwa_erros-type       = gwa_return-type.
*      lwa_erros-number     = gwa_return-number.
*      lwa_erros-message    = gwa_return-message.
*      lwa_erros-message_v1 = gwa_return-message_v1.
*      lwa_erros-message_v2 = gwa_return-message_v2.
*      lwa_erros-message_v3 = gwa_return-message_v3.
*
*      APPEND  lwa_erros TO git_erros.
      CLEAR: lwa_erros.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*      EXIT.
    ENDIF.
    CLEAR: gwa_datageneral.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_MODIFICA_EQPTO_FROTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_modifica_eqpto_frota .
  DATA: lva_equipment LIKE bapi_itob_parms-equipment,
        lva_wait      LIKE bapita-wait,
        lva_return    TYPE bapireturn,
        lva_linha     TYPE sy-tabix,
        lwa_erros     LIKE LINE OF git_erros.

  CLEAR:  git_erros, gwa_datageneral.

  LOOP AT  git_saida_cat02_m ASSIGNING FIELD-SYMBOL(<lwa_saida_cat02_m>).
    lva_linha = sy-tabix.
    CLEAR: gva_valid_date, gva_equipment, gwa_return, gwa_datageneral, gwa_datafleet, gwa_dataspecific.

    gva_equipment = <lwa_saida_cat02_m>-equipment.

    IF <lwa_saida_cat02_m>-valid_date IS INITIAL.
      gva_valid_date = sy-datum.
    ELSE.
      gva_valid_date = <lwa_saida_cat02_m>-valid_date.
    ENDIF.

    gwa_datageneral-authgrp                =   <lwa_saida_cat02_m>-authgrp.
    gwa_datageneral-obj_weight             =   <lwa_saida_cat02_m>-obj_weight.
    gwa_datageneral-unit_of_wt             =   <lwa_saida_cat02_m>-unit_of_wt.
    gwa_datageneral-obj_size               =   <lwa_saida_cat02_m>-obj_size.
    gwa_datageneral-inventory              =   <lwa_saida_cat02_m>-inventory.
    gwa_datageneral-start_from             =   <lwa_saida_cat02_m>-start_from.
    gwa_datageneral-objecttype             =   <lwa_saida_cat02_m>-objecttype.
    gwa_datageneral-acquisval              =   <lwa_saida_cat02_m>-acquisval.
    gwa_datageneral-currency               =   <lwa_saida_cat02_m>-currency.
    gwa_datageneral-acqdate                =   <lwa_saida_cat02_m>-acqdate.
    gwa_datageneral-manfacture             =   <lwa_saida_cat02_m>-manfacture.
    gwa_datageneral-mancountry             =   <lwa_saida_cat02_m>-mancountry.
    gwa_datageneral-manmodel               =   <lwa_saida_cat02_m>-manmodel.
    gwa_datageneral-constyear              =   <lwa_saida_cat02_m>-constyear.
    gwa_datageneral-constmonth             =   <lwa_saida_cat02_m>-constmonth.
    gwa_datageneral-manparno               =   <lwa_saida_cat02_m>-manparno.
    gwa_datageneral-manserno               =   <lwa_saida_cat02_m>-manserno.
    gwa_datageneral-descript               =   <lwa_saida_cat02_m>-descript.
    gwa_datageneral-start_from             =   <lwa_saida_cat02_m>-start_from.
    gwa_datageneral-maintplant             =   <lwa_saida_cat02_m>-maintplant.
    gwa_datageneral-maintloc               =   <lwa_saida_cat02_m>-maintloc.
    gwa_datageneral-maintroom              =   <lwa_saida_cat02_m>-maintroom . "Teste Sala - ABAP
    gwa_datageneral-plsectn                =   <lwa_saida_cat02_m>-plsectn.
    gwa_datageneral-pp_wkctr               =   <lwa_saida_cat02_m>-pp_wkctr.
    gwa_datageneral-abcindic               =   <lwa_saida_cat02_m>-abcindic.
    gwa_datageneral-sortfield              =   <lwa_saida_cat02_m>-sortfield.
    gwa_datageneral-comp_code              =   <lwa_saida_cat02_m>-comp_code.
    gwa_datageneral-bus_area               =   <lwa_saida_cat02_m>-bus_area.
    gwa_datageneral-asset_no               =   <lwa_saida_cat02_m>-asset_no.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-asset_no
      IMPORTING
        output = gwa_datageneral-asset_no.

    gwa_datageneral-costcenter             =   <lwa_saida_cat02_m>-costcenter.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-costcenter
      IMPORTING
        output = gwa_datageneral-costcenter.

    gwa_datageneral-wbs_elem               =   <lwa_saida_cat02_m>-wbs_elem.

    gwa_datageneral-standorder             =   <lwa_saida_cat02_m>-standorder.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-standorder
      IMPORTING
        output = gwa_datageneral-standorder.


    gwa_datageneral-settlorder             =   <lwa_saida_cat02_m>-settlorder.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-settlorder
      IMPORTING
        output = gwa_datageneral-settlorder.

    gwa_datageneral-planplant              =   <lwa_saida_cat02_m>-planplant.
    gwa_datageneral-plangroup              =   <lwa_saida_cat02_m>-plangroup.
    gwa_datageneral-work_ctr               =   <lwa_saida_cat02_m>-work_ctr.
    gwa_datageneral-catprofile             =   <lwa_saida_cat02_m>-catprofile.
    gwa_datageneral-consttype              =   <lwa_saida_cat02_m>-consttype.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_datageneral-consttype
      IMPORTING
        output = gwa_datageneral-consttype.


    gwa_dataspecific-inst_pos     =   <lwa_saida_cat02_m>-inst_pos.
    gwa_dataspecific-techid       =   <lwa_saida_cat02_m>-techid.
    gwa_dataspecific-equicatgry   =   <lwa_saida_cat02_m>-equicatgry.
    gwa_dataspecific-read_floc    =   <lwa_saida_cat02_m>-read_floc.


    gwa_datafleet-fleet_num       =   <lwa_saida_cat02_m>-fleet_num.
    gwa_datafleet-license_num     =   <lwa_saida_cat02_m>-license_num.
    gwa_datafleet-expiry_date     =   <lwa_saida_cat02_m>-expiry_date.
    gwa_datafleet-fleet_vin       =   <lwa_saida_cat02_m>-fleet_vin.
    gwa_datafleet-chassis_num     =   <lwa_saida_cat02_m>-chassis_num.
    gwa_datafleet-gross_wgt       =   <lwa_saida_cat02_m>-gross_wgt.
    gwa_datafleet-load_wgt        =   <lwa_saida_cat02_m>-load_wgt.
    gwa_datafleet-load_vol        =   <lwa_saida_cat02_m>-load_vol.
    gwa_datafleet-vol_unit        =   <lwa_saida_cat02_m>-vol_unit.
    gwa_datafleet-load_hgt        =   <lwa_saida_cat02_m>-load_hgt.
    gwa_datafleet-load_dim_unit   =   <lwa_saida_cat02_m>-load_dim_unit.
    gwa_datafleet-load_wid        =   <lwa_saida_cat02_m>-load_wid.
    gwa_datafleet-load_len        =   <lwa_saida_cat02_m>-load_len.
    gwa_datafleet-no_compart      =   <lwa_saida_cat02_m>-no_compart.
    gwa_datafleet-fleet_hgt       =   <lwa_saida_cat02_m>-fleet_hgt.
    gwa_datafleet-dim_unit        =   <lwa_saida_cat02_m>-dim_unit.
    gwa_datafleet-fleet_wid       =   <lwa_saida_cat02_m>-fleet_wid.
    gwa_datafleet-fleet_len       =   <lwa_saida_cat02_m>-fleet_len.
    gwa_datafleet-repla_date      =   <lwa_saida_cat02_m>-repla_date.
    gwa_datafleet-repla_odom      =   <lwa_saida_cat02_m>-repla_odom.
    gwa_datafleet-repla_oph       =   <lwa_saida_cat02_m>-repla_oph.
    gwa_datafleet-fleet_use       =   <lwa_saida_cat02_m>-fleet_use.
    gwa_datafleet-card_num        =   <lwa_saida_cat02_m>-card_num.
    gwa_datafleet-max_occupants   =   <lwa_saida_cat02_m>-max_occupants.
    gwa_datafleet-key_num         =   <lwa_saida_cat02_m>-key_num.
    gwa_datafleet-num_axle        =   <lwa_saida_cat02_m>-num_axle.
    gwa_datafleet-engine_type     =   <lwa_saida_cat02_m>-engine_type.
    gwa_datafleet-engine_snr      =   <lwa_saida_cat02_m>-engine_snr.
    gwa_datafleet-speed_max       =   <lwa_saida_cat02_m>-speed_max.
    gwa_datafleet-speed_unit      =   <lwa_saida_cat02_m>-speed_unit.
    gwa_datafleet-engine_power    =   <lwa_saida_cat02_m>-engine_power.
    gwa_datafleet-unit_power      =   <lwa_saida_cat02_m>-unit_power.
    gwa_datafleet-revolutions     =   <lwa_saida_cat02_m>-revolutions.
    gwa_datafleet-engine_cap      =   <lwa_saida_cat02_m>-engine_cap.
    gwa_datafleet-unit_cap        =   <lwa_saida_cat02_m>-unit_cap.
    gwa_datafleet-engine_cyl      =   <lwa_saida_cat02_m>-engine_cyl.
    gwa_datafleet-fuel_pri        =   <lwa_saida_cat02_m>-fuel_pri.
    gwa_datafleet-fuel_sec        =   <lwa_saida_cat02_m>-fuel_sec.
    gwa_datafleet-oil_type        =   <lwa_saida_cat02_m>-oil_type.
    gwa_datafleet-pri_calc        =   <lwa_saida_cat02_m>-pri_calc.


    APPEND VALUE #( structure = 'DIV1' valuepart1 = '07116DD809592015' ) TO git_xtensionin.
    APPEND VALUE #( structure = 'DIV2' valuepart1 = '07116DD809592015' ) TO git_xtensionin.
    APPEND VALUE #( structure = 'DIV3' valuepart1 = '07116DD809592015' ) TO git_xtensionin.
    APPEND VALUE #( structure = 'TQ_COMBUSTIVEL_1' valuepart1 = '120' )  TO git_xtensionin.
    APPEND VALUE #( structure = 'TQ_COMBUSTIVEL_2' valuepart1 = '120' )  TO git_xtensionin.
    APPEND VALUE #( structure = 'TQ_COMBUSTIVEL_3' valuepart1 = '120' )  TO git_xtensionin.


    IF <lwa_saida_cat02_m>-standorder IS NOT INITIAL.
      gwa_data_generalx-standorder = 'X'.
    ENDIF.

    IF <lwa_saida_cat02_m>-settlorder IS NOT INITIAL.
      gwa_data_generalx-settlorder = 'X'.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gva_equipment
      IMPORTING
        output = gva_equipment.

    CALL FUNCTION 'BAPI_EQUI_CHANGE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        equipment      = gva_equipment
        data_general   = gwa_datageneral
        data_generalx  = gwa_data_generalx
        data_specific  = gwa_dataspecific
        data_specificx = gwa_data_specificx
        data_fleet     = gwa_datafleet
        data_fleetx    = gwa_data_fleetx
        valid_date     = sy-datum
        valid_time     = sy-uzeit
      IMPORTING
        return         = gwa_return
      TABLES
        extensionin    = git_xtensionin.

    IF gwa_return IS INITIAL.
      lva_wait = 'X' .
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = lva_wait.

      <lwa_saida_cat02_m>-status = '@08@'.
    ELSE.
      <lwa_saida_cat02_m>-status = '@0A@'.
*      txt = 'Tipo de equipamento, diferença da tela de seleção'.
      APPEND VALUE #( id = <lwa_saida_cat02_m>-id status = '@0A@' type = 'E' desc_status = gwa_return-message ) TO t_status.
*
*
*      lwa_erros-linha      = lva_linha.
*      lwa_erros-id         = gwa_return-id(2).
*      lwa_erros-type       = gwa_return-type.
*      lwa_erros-number     = gwa_return-number.
*      lwa_erros-message    = gwa_return-message.
*      lwa_erros-message_v1 = gwa_return-message_v1.
*      lwa_erros-message_v2 = gwa_return-message_v2.
*      lwa_erros-message_v3 = gwa_return-message_v3.
*
*      APPEND  lwa_erros TO git_erros.
*      CLEAR: lwa_erros.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    ENDIF.
    CLEAR: gwa_datageneral.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_MONTA_EQPTO_LOCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LVA_EQUIPMENT  text
*----------------------------------------------------------------------*
FORM fm_monta_eqpto_local  USING p_equipment p_maintloc.
  DATA: funcloc             TYPE tplnr,
        wa_return           TYPE bapiret2,
        wa_return_bapi_eqmt TYPE bapireturn.

  CLEAR: funcloc, wa_return_bapi_eqmt, wa_return.


  CALL FUNCTION 'BAPI_EQMT_INSTALLFL' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      equipment = p_equipment
      funcloc   = p_maintloc
      date      = sy-datum
      time      = sy-uzeit
    IMPORTING
      return    = wa_return_bapi_eqmt.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait   = 'X'
    IMPORTING
      return = wa_return.
*  WAIT UP TO 2 SECONDS.
ENDFORM.
FORM fm_set_dados_loc_inst.

  DATA: ls_saida_local_inst_001 LIKE LINE OF git_saida_local_inst_001,
        ls_saida_local_inst_002 LIKE LINE OF git_saida_local_inst_002.

  DATA: l_compos TYPE i,
        l_comlen TYPE i,
        l_dotpos TYPE i.

  DATA: l_type TYPE c LENGTH 1.

  FREE: git_saida_local_inst_001, git_saida_local_inst_002.

  DELETE it_excel WHERE row EQ 1.
  DELETE it_excel WHERE row EQ 2.

  CASE p_tipo.
    WHEN '0001'.
      LOOP AT it_excel INTO DATA(wa_excel).

        AT NEW row.
          IF ls_saida_local_inst_001-grupo IS NOT INITIAL AND ls_saida_local_inst_001-empresa IS NOT INITIAL AND
             ls_saida_local_inst_001-centro IS NOT INITIAL AND ls_saida_local_inst_001-setor IS NOT INITIAL.
            APPEND ls_saida_local_inst_001 TO git_saida_local_inst_001.
          ENDIF.
          CLEAR: ls_saida_local_inst_001.
        ENDAT.

        ASSIGN COMPONENT wa_excel-col OF STRUCTURE ls_saida_local_inst_001 TO FIELD-SYMBOL(<value_excel_001>).
        IF <value_excel_001> IS ASSIGNED.

          DESCRIBE FIELD <value_excel_001> TYPE l_type.

          CASE l_type.
            WHEN 'P'.

              REPLACE ALL OCCURRENCES OF '.' IN wa_excel-value-feld WITH space.
              REPLACE ALL OCCURRENCES OF ',' IN wa_excel-value-feld WITH '.'.
              CONDENSE  wa_excel-value-feld NO-GAPS.

              <value_excel_001> = wa_excel-value-feld.
            WHEN 'D'.
              REPLACE ALL OCCURRENCES OF '.' IN wa_excel-value-feld WITH space.
              CONDENSE wa_excel-value-feld NO-GAPS.
              CONCATENATE wa_excel-value-feld+4(4) wa_excel-value-feld+2(2) wa_excel-value-feld(2) INTO <value_excel_001>.
            WHEN OTHERS.
              <value_excel_001> = wa_excel-value-feld.
          ENDCASE.

        ENDIF.

      ENDLOOP.
    WHEN '0002'.
      LOOP AT it_excel INTO wa_excel.

        AT NEW row.
          IF ls_saida_local_inst_002-grupo IS NOT INITIAL AND ls_saida_local_inst_002-empresa IS NOT INITIAL AND
             ls_saida_local_inst_002-centro IS NOT INITIAL AND ls_saida_local_inst_002-setor IS NOT INITIAL.
            APPEND ls_saida_local_inst_002 TO git_saida_local_inst_002.
          ENDIF.
          CLEAR: ls_saida_local_inst_002.
        ENDAT.

        ASSIGN COMPONENT wa_excel-col OF STRUCTURE ls_saida_local_inst_002 TO FIELD-SYMBOL(<value_excel_002>).
        IF <value_excel_002> IS ASSIGNED.

          DESCRIBE FIELD <value_excel_002> TYPE l_type.

          CASE l_type.
            WHEN 'P'.

              REPLACE ALL OCCURRENCES OF '.' IN wa_excel-value-feld WITH space.
              REPLACE ALL OCCURRENCES OF ',' IN wa_excel-value-feld WITH '.'.

              <value_excel_002> = wa_excel-value-feld.
            WHEN 'D'.
              REPLACE ALL OCCURRENCES OF '.' IN wa_excel-value-feld WITH space.
              CONDENSE wa_excel-value-feld NO-GAPS.
              CONCATENATE wa_excel-value-feld+4(4) wa_excel-value-feld+2(2) wa_excel-value-feld(2) INTO <value_excel_002>.
            WHEN OTHERS.
              <value_excel_002> = wa_excel-value-feld.
          ENDCASE.
        ENDIF.

      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

FORM fm_set_dados_mod_loc_inst.


  DATA: ls_saida_local_inst_001 LIKE LINE OF git_saida_local_inst_001,
        ls_saida_local_inst_002 LIKE LINE OF git_saida_local_inst_002.

  DATA: l_compos TYPE i,
        l_comlen TYPE i,
        l_dotpos TYPE i.

  DATA: l_type TYPE c LENGTH 1.

  FREE: git_saida_local_inst_001, git_saida_local_inst_002.

  DELETE it_excel WHERE row EQ 1.
  DELETE it_excel WHERE row EQ 2.

  CASE p_tipo.
    WHEN '0001'.
      LOOP AT it_excel INTO DATA(wa_excel).

        AT NEW row.
          IF ls_saida_local_inst_001-funcloc IS NOT INITIAL.
            APPEND ls_saida_local_inst_001 TO git_saida_local_inst_001.
          ENDIF.
          CLEAR: ls_saida_local_inst_001.
        ENDAT.

        wa_excel-col = wa_excel-col + 6.

        ASSIGN COMPONENT wa_excel-col OF STRUCTURE ls_saida_local_inst_001 TO FIELD-SYMBOL(<value_excel_001>).
        IF <value_excel_001> IS ASSIGNED.

          DESCRIBE FIELD <value_excel_001> TYPE l_type.

          CASE l_type.
            WHEN 'P'.

              REPLACE ALL OCCURRENCES OF '.' IN wa_excel-value-feld WITH space.
              REPLACE ALL OCCURRENCES OF ',' IN wa_excel-value-feld WITH '.'.
              CONDENSE  wa_excel-value-feld NO-GAPS.

              <value_excel_001> = wa_excel-value-feld.
            WHEN 'D'.
              REPLACE ALL OCCURRENCES OF '.' IN wa_excel-value-feld WITH space.
              CONDENSE wa_excel-value-feld NO-GAPS.
              CONCATENATE wa_excel-value-feld+4(4) wa_excel-value-feld+2(2) wa_excel-value-feld(2) INTO <value_excel_001>.
            WHEN OTHERS.
              <value_excel_001> = wa_excel-value-feld.
          ENDCASE.

        ENDIF.

      ENDLOOP.
    WHEN '0002'.
      LOOP AT it_excel INTO wa_excel.

        AT NEW row.
          IF ls_saida_local_inst_002-funcloc IS NOT INITIAL.
            APPEND ls_saida_local_inst_002 TO git_saida_local_inst_002.
          ENDIF.
          CLEAR: ls_saida_local_inst_002.
        ENDAT.

        wa_excel-col = wa_excel-col + 6.

        ASSIGN COMPONENT wa_excel-col OF STRUCTURE ls_saida_local_inst_002 TO FIELD-SYMBOL(<value_excel_002>).
        IF <value_excel_002> IS ASSIGNED.

          DESCRIBE FIELD <value_excel_002> TYPE l_type.

          CASE l_type.
            WHEN 'P'.

              REPLACE ALL OCCURRENCES OF '.' IN wa_excel-value-feld WITH space.
              REPLACE ALL OCCURRENCES OF ',' IN wa_excel-value-feld WITH '.'.

              CONDENSE  wa_excel-value-feld NO-GAPS.
              <value_excel_002> = wa_excel-value-feld.
            WHEN 'D'.
              REPLACE ALL OCCURRENCES OF '.' IN wa_excel-value-feld WITH space.
              CONDENSE wa_excel-value-feld NO-GAPS.
              CONCATENATE wa_excel-value-feld+4(4) wa_excel-value-feld+2(2) wa_excel-value-feld(2) INTO <value_excel_002>.
            WHEN OTHERS.
              <value_excel_002> = wa_excel-value-feld.
          ENDCASE.
        ENDIF.

      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
FORM fm_check_inform_locinst_criar.

  DATA: lwa_erros   LIKE LINE OF git_erros,
        lva_linha   TYPE sy-tabix,
        lva_exit    TYPE c,
        lva_fun_loc TYPE  iflo-tplnr,
        lva_x       TYPE i,
        lva_string  TYPE string,
        txt         TYPE char50.

  DATA: l_location_install_key TYPE c LENGTH 30,
        l_search_key           TYPE c LENGTH 30,
        l_key_sequence         TYPE numc2.

  FREE: t_status.
  CLEAR: p_erro.

  FIELD-SYMBOLS: <lva_field> TYPE any.

  gva_verificado = ''.
  gva_erro = ''.
  CLEAR: git_erros[].

  CASE p_tipo.
    WHEN '0001'.
      LOOP AT git_saida_local_inst_001 ASSIGNING FIELD-SYMBOL(<local_inst_001>).
        DATA(l_tabix) = sy-tabix.

        IF <local_inst_001>-setor IS NOT INITIAL.
          SELECT COUNT(*) FROM zpmt001
           WHERE codigo = <local_inst_001>-setor.
          IF sy-subrc IS NOT INITIAL.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = 'O setor informado não existe no "Cadastro de setor" ' ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ELSE.
          txt = |Preencha o setor|.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF <local_inst_001>-eqsup IS NOT INITIAL.
          SELECT COUNT(*) FROM zpmt002
           WHERE codigo = <local_inst_001>-eqsup
             AND eqhier = 1.
          IF sy-subrc IS NOT INITIAL.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = 'O tipo de equipamento informado não existe no "Cadastro de tipo de equipamento".' ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ELSE.
          txt = |Preencha o equipamento superior|.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF p_erro = abap_false.
          CLEAR: l_location_install_key, l_search_key.
          l_location_install_key = |{ <local_inst_001>-grupo }.{ <local_inst_001>-empresa }.{ <local_inst_001>-centro }.{ <local_inst_001>-setor }.{ <local_inst_001>-eqsup }|.
          l_search_key           = l_location_install_key && '%'.

          SELECT tplnr pltxt
            FROM iflo
            INTO TABLE locations
           WHERE tplnr LIKE l_search_key ORDER BY PRIMARY KEY.

          DELETE ADJACENT DUPLICATES FROM locations COMPARING tplnr.
          SORT locations BY tplnr+22(2).

          IF ( NOT locations IS INITIAL ).
            l_key_sequence = locations[ lines( locations ) ]-tplnr+22(2).
            ADD 1 TO l_key_sequence.
          ELSE.
            l_key_sequence = 01.
          ENDIF.

          l_location_install_key   = l_location_install_key && l_key_sequence.
          <local_inst_001>-funcloc = l_location_install_key.
        ENDIF.

        IF <local_inst_001>-strind NE p_tplkz.
          txt = 'Código estrutura difere do valor informado em tela'.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF <local_inst_001>-category NE p_fltyp.
          txt = 'Categoria local de instalação difere do valor informado em tela'.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF <local_inst_001>-manmodel IS INITIAL.
          txt = 'Preencha a denominação do tipo atribuído pelo fabricante'.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF <local_inst_001>-unit_of_wt IS NOT INITIAL.
          SELECT COUNT(*) FROM t006b WHERE mseh3 EQ <local_inst_001>-unit_of_wt AND spras EQ sy-langu.
          IF sy-subrc NE 0.
            txt = |Unidade peso { <local_inst_001>-unit_of_wt } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        IF <local_inst_001>-mancountry IS NOT INITIAL.
          SELECT COUNT(*) FROM t005 WHERE land1 EQ <local_inst_001>-mancountry.
          IF sy-subrc NE 0.
            txt = |Unidade do pais { <local_inst_001>-mancountry } não cadastrada|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        IF <local_inst_001>-authgrp IS NOT INITIAL.
          <local_inst_001>-authgrp = |{ <local_inst_001>-authgrp ALPHA = IN }|.
          SELECT COUNT(*) FROM t370b WHERE begru EQ <local_inst_001>-authgrp.
          IF sy-subrc NE 0.
            txt = |Grupo de autorização { <local_inst_001>-authgrp  } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ELSE.
          txt = |Preencha o grupo de autorização|.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF <local_inst_001>-currency IS NOT INITIAL.
          SELECT COUNT(*) FROM tcurc WHERE waers EQ <local_inst_001>-currency.
          IF sy-subrc NE 0.
            txt = |Código da moéda { <local_inst_001>-currency  } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        IF <local_inst_001>-maintloc IS NOT INITIAL.
          SELECT COUNT(*) FROM t499s WHERE werks EQ <local_inst_001>-centro AND stand EQ <local_inst_001>-maintloc.
          IF sy-subrc NE 0.
            txt = |Localização do imobilizado { <local_inst_001>-maintloc } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        IF <local_inst_001>-plsectn  IS NOT INITIAL.
          <local_inst_001>-plsectn = |{ <local_inst_001>-plsectn ALPHA = IN }|.
          SELECT COUNT(*) FROM t357 WHERE werks EQ <local_inst_001>-centro AND beber EQ <local_inst_001>-plsectn.
          IF sy-subrc NE 0.
            txt = |Area operacional { <local_inst_001>-plsectn  } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        IF <local_inst_001>-work_ctr IS NOT INITIAL.
          SELECT COUNT(*) FROM m_cramv WHERE werks EQ <local_inst_001>-centro AND arbpl EQ <local_inst_001>-work_ctr.
          IF sy-subrc NE 0.
            txt = |Centro de trabalho { <local_inst_001>-work_ctr } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        IF <local_inst_001>-abcindic IS NOT INITIAL.
          SELECT COUNT(*) FROM t370c_t WHERE spras EQ sy-langu AND abckz EQ <local_inst_001>-abcindic.
          IF sy-subrc NE 0.
            txt = |Código ABC { <local_inst_001>-abcindic } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        IF <local_inst_001>-asset_no IS NOT INITIAL.
          <local_inst_001>-asset_no = |{ <local_inst_001>-asset_no ALPHA = IN }|.
          <local_inst_001>-comp_code = |{ <local_inst_001>-comp_code ALPHA = IN }|.
          SELECT COUNT(*) FROM anla WHERE anln1 EQ <local_inst_001>-asset_no AND bukrs EQ <local_inst_001>-comp_code.
          IF sy-subrc NE 0.
            txt = |Imobilizado { <local_inst_001>-asset_no } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        IF <local_inst_001>-costcenter IS NOT INITIAL.
          <local_inst_001>-costcenter = |{ <local_inst_001>-costcenter ALPHA = IN }|.
          <local_inst_001>-comp_code = |{ <local_inst_001>-comp_code ALPHA = IN }|.
          SELECT COUNT(*) FROM m_kostn WHERE kostl  EQ <local_inst_001>-costcenter AND spras EQ sy-langu AND bukrs EQ <local_inst_001>-comp_code.
          IF sy-subrc NE 0.
            txt = |Centro de custo { <local_inst_001>-costcenter } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ELSE.
          txt = |Preencha o centro de custo|.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF <local_inst_001>-plangroup IS NOT INITIAL.
          SELECT COUNT(*) FROM t024i WHERE iwerk  EQ <local_inst_001>-centro AND ingrp EQ <local_inst_001>-plangroup.
          IF sy-subrc NE 0.
            txt = |Grupo de planejamento { <local_inst_001>-plangroup } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        IF <local_inst_001>-consttype IS NOT INITIAL.
          <local_inst_001>-consttype = |{ <local_inst_001>-consttype ALPHA  = IN }|.
          SELECT COUNT(*) FROM m_mat1j WHERE matnr EQ <local_inst_001>-consttype AND spras EQ sy-langu.
          IF sy-subrc NE 0.
            txt = |Conjunto { <local_inst_001>-consttype } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        IF p_erro IS INITIAL.
          txt = 'Erros não encontrado'.
          APPEND VALUE #( id = l_tabix status = '@08@' type = 'S' desc_status = txt ) TO t_status.
        ENDIF.

        CLEAR: p_erro.
      ENDLOOP.
    WHEN '0002'.
      LOOP AT git_saida_local_inst_002 ASSIGNING FIELD-SYMBOL(<local_inst_002>).
        l_tabix = sy-tabix.

        IF <local_inst_002>-setor IS NOT INITIAL.
          SELECT COUNT(*) FROM zpmt001
           WHERE codigo = <local_inst_002>-setor.
          IF sy-subrc IS NOT INITIAL.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = 'O setor informado não existe no "Cadastro de setor" ' ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ELSE.
          txt = |Preencha o setor|.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF <local_inst_002>-eqsup IS NOT INITIAL.
          SELECT COUNT(*) FROM zpmt002
           WHERE codigo = <local_inst_002>-eqsup
             AND eqhier = 1.
          IF sy-subrc IS NOT INITIAL.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = 'O tipo de equipamento informado não existe no "Cadastro de tipo de equipamento".' ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ELSE.
          txt = |Preencha o equipamento superior|.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF <local_inst_002>-eqinf IS NOT INITIAL.
          SELECT COUNT(*) FROM zpmt002
           WHERE codigo = <local_inst_002>-eqinf
             AND eqhier = 2.
          IF sy-subrc IS NOT INITIAL OR strlen( <local_inst_002>-eqinf ) > 3.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = 'O tipo de equipamento informado não existe no "Cadastro de tipo de equipamento".' ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ELSE.
          txt = |Preencha o equipamento inferior|.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF p_erro = abap_false.
          CLEAR: l_location_install_key, l_search_key.
          l_location_install_key = |{ <local_inst_002>-grupo }.{ <local_inst_002>-empresa }.{ <local_inst_002>-centro }.{ <local_inst_002>-setor }.{ <local_inst_002>-eqsup }.{ <local_inst_002>-eqinf }|.
          l_search_key           = l_location_install_key && '%'.

          SELECT tplnr pltxt
            FROM iflo
            INTO TABLE locations
           WHERE tplnr LIKE l_search_key ORDER BY PRIMARY KEY .

          DELETE ADJACENT DUPLICATES FROM locations COMPARING tplnr.
          SORT locations BY tplnr+28(2).

          IF ( NOT locations IS INITIAL ).
            l_key_sequence = locations[ lines( locations ) ]-tplnr+28(2).
            ADD 1 TO l_key_sequence.
          ELSE.
            l_key_sequence = 01.
          ENDIF.

          l_location_install_key   = l_location_install_key && l_key_sequence.
          <local_inst_002>-funcloc = l_location_install_key.
        ENDIF.

        IF <local_inst_002>-strind NE p_tplkz.
          txt = 'Código estrutura difere do valor informado em tela'.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF <local_inst_002>-category NE p_fltyp.
          txt = 'Categoria local de instalação difere do valor informado em tela'.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF <local_inst_002>-manmodel IS INITIAL.
          txt = 'Preencha a denominação do tipo atribuído pelo fabricante'.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF <local_inst_002>-unit_of_wt IS NOT INITIAL.
          SELECT COUNT(*) FROM t006b WHERE mseh3 EQ <local_inst_002>-unit_of_wt AND spras EQ sy-langu.
          IF sy-subrc NE 0.
            txt = |Unidade peso { <local_inst_002>-unit_of_wt } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        IF <local_inst_002>-mancountry IS NOT INITIAL.
          SELECT COUNT(*) FROM t005 WHERE land1 EQ <local_inst_002>-mancountry.
          IF sy-subrc NE 0.
            txt = |Unidade do pais { <local_inst_002>-mancountry } não cadastrada|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        IF <local_inst_002>-authgrp IS NOT INITIAL.
          <local_inst_002>-authgrp = |{ <local_inst_002>-authgrp ALPHA = IN }|.
          SELECT COUNT(*) FROM t370b WHERE begru EQ <local_inst_002>-authgrp.
          IF sy-subrc NE 0.
            txt = |Grupo de autorização { <local_inst_002>-authgrp  } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ELSE.
          txt = |Preencha o grupo de autorização|.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF <local_inst_002>-currency IS NOT INITIAL.
          SELECT COUNT(*) FROM tcurc WHERE waers EQ <local_inst_002>-currency.
          IF sy-subrc NE 0.
            txt = |Código da moéda { <local_inst_002>-currency  } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        IF <local_inst_002>-maintloc IS NOT INITIAL.
          SELECT COUNT(*) FROM t499s WHERE werks EQ <local_inst_002>-centro AND stand EQ <local_inst_002>-maintloc.
          IF sy-subrc NE 0.
            txt = |Localização do imobilizado { <local_inst_002>-maintloc } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        IF <local_inst_002>-plsectn  IS NOT INITIAL.
          <local_inst_002>-plsectn = |{ <local_inst_002>-plsectn ALPHA = IN }|.
          SELECT COUNT(*) FROM t357 WHERE werks EQ <local_inst_002>-centro AND beber EQ <local_inst_002>-plsectn.
          IF sy-subrc NE 0.
            txt = |Area operacional { <local_inst_002>-plsectn  } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        IF <local_inst_002>-work_ctr IS NOT INITIAL.
          SELECT COUNT(*) FROM m_cramv WHERE werks EQ <local_inst_002>-centro AND arbpl EQ <local_inst_002>-work_ctr.
          IF sy-subrc NE 0.
            txt = |Centro de trabalho { <local_inst_002>-work_ctr } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        IF <local_inst_002>-abcindic IS NOT INITIAL.
          SELECT COUNT(*) FROM t370c_t WHERE spras EQ sy-langu AND abckz EQ <local_inst_002>-abcindic.
          IF sy-subrc NE 0.
            txt = |Código ABC { <local_inst_002>-abcindic } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        IF <local_inst_002>-asset_no IS NOT INITIAL.
          <local_inst_002>-asset_no  = |{ <local_inst_002>-asset_no  ALPHA = IN }|.
          <local_inst_002>-comp_code = |{ <local_inst_002>-comp_code ALPHA = IN }|.
          SELECT COUNT(*) FROM anla WHERE anln1 EQ <local_inst_002>-asset_no AND bukrs EQ <local_inst_002>-comp_code.
          IF sy-subrc NE 0.
            txt = |Imobilizado { <local_inst_002>-asset_no } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        IF <local_inst_002>-costcenter IS NOT INITIAL.
          <local_inst_002>-costcenter = |{ <local_inst_002>-costcenter ALPHA = IN }|.
          <local_inst_002>-comp_code  = |{ <local_inst_002>-comp_code ALPHA = IN }|.
          SELECT COUNT(*) FROM m_kostn WHERE kostl  EQ <local_inst_002>-costcenter AND spras EQ sy-langu AND bukrs EQ <local_inst_002>-comp_code.
          IF sy-subrc NE 0.
            txt = |Centro de custo { <local_inst_002>-costcenter } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ELSE.
          txt = |Preencha o centro de custo|.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF <local_inst_002>-plangroup IS NOT INITIAL.
          SELECT COUNT(*) FROM t024i WHERE iwerk  EQ <local_inst_002>-centro AND ingrp EQ <local_inst_002>-plangroup.
          IF sy-subrc NE 0.
            txt = |Grupo de planejamento { <local_inst_002>-plangroup } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        IF <local_inst_002>-consttype IS NOT INITIAL.
          <local_inst_002>-consttype = |{ <local_inst_002>-consttype ALPHA  = IN }|.
          SELECT COUNT(*) FROM m_mat1j WHERE matnr EQ <local_inst_002>-consttype AND spras EQ sy-langu.
          IF sy-subrc NE 0.
            txt = |Conjunto { <local_inst_002>-consttype } não cadastrado|.
            APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
            p_erro = abap_true.
          ENDIF.
        ENDIF.

        IF p_erro IS INITIAL.
          txt = 'Erros não encontrado'.
          APPEND VALUE #( id = l_tabix status = '@08@' type = 'S' desc_status = txt ) TO t_status.
        ENDIF.

        CLEAR: p_erro.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

  IF t_status[] IS NOT INITIAL.
    PERFORM f_alv_log.
  ENDIF.

ENDFORM.
FORM fm_check_inform_locinst_mod.

  DATA: lwa_erros   LIKE LINE OF git_erros,
        lva_linha   TYPE sy-tabix,
        lva_exit    TYPE c,
        lva_fun_loc TYPE  iflo-tplnr,
        lva_x       TYPE i,
        lva_string  TYPE string,
        txt         TYPE char50.

  DATA: l_location_install_key TYPE c LENGTH 30,
        l_search_key           TYPE c LENGTH 30,
        l_key_sequence         TYPE numc2.

  FREE: t_status.
  CLEAR: p_erro.

  FIELD-SYMBOLS: <lva_field> TYPE any.

  gva_verificado = ''.
  gva_erro = ''.
  CLEAR: git_erros[].

  CASE p_tipo.
    WHEN '0001'.
      LOOP AT git_saida_local_inst_001 ASSIGNING FIELD-SYMBOL(<local_inst_001>).
        DATA(l_tabix) = sy-tabix.

        IF <local_inst_001>-strind NE p_tplkz.
          txt = 'Código estrutura difere do valor informado em tela'.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF <local_inst_001>-category NE p_fltyp.
          txt = 'Categoria local de instalação difere do valor informado em tela'.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF <local_inst_001>-funcloc IS INITIAL.
          txt = 'Preencha o numero do local de instalação'.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF p_erro IS INITIAL.
          txt = 'Erros não encontrado'.
          APPEND VALUE #( id = l_tabix status = '@08@' type = 'S' desc_status = txt ) TO t_status.
        ENDIF.

        CLEAR: p_erro.
      ENDLOOP.
    WHEN '0002'.
      LOOP AT git_saida_local_inst_002 ASSIGNING FIELD-SYMBOL(<local_inst_002>).
        l_tabix = sy-tabix.

        IF <local_inst_002>-strind NE p_tplkz.
          txt = 'Código estrutura difere do valor informado em tela'.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF <local_inst_002>-category NE p_fltyp.
          txt = 'Categoria local de instalação difere do valor informado em tela'.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF <local_inst_002>-funcloc IS INITIAL.
          txt = 'Preencha o numero do local de instalação'.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = txt ) TO t_status.
          p_erro = abap_true.
        ENDIF.

        IF p_erro IS INITIAL.
          txt = 'Erros não encontrado'.
          APPEND VALUE #( id = l_tabix status = '@08@' type = 'S' desc_status = txt ) TO t_status.
        ENDIF.

        CLEAR: p_erro.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

  IF t_status[] IS NOT INITIAL.
    PERFORM f_alv_log.
  ENDIF.

ENDFORM.

FORM f_alv_log.

  DATA: lt_field TYPE STANDARD TABLE OF slis_fieldcat_alv,
        ls_field TYPE slis_fieldcat_alv.

  ls_field-col_pos   = 1.
  ls_field-fieldname = 'ID'.
  ls_field-seltext_m = 'ID'.
  APPEND ls_field TO lt_field.
  CLEAR ls_field.

  ls_field-col_pos = 2.
  ls_field-fieldname = 'STATUS'.
  ls_field-seltext_m = 'Status'.
  APPEND ls_field TO lt_field.
  CLEAR ls_field.

  ls_field-col_pos = 3.
  ls_field-fieldname = 'DESC_STATUS'.
  ls_field-seltext_m = 'Mensagem'.
  ls_field-outputlen = 70.
  APPEND ls_field TO lt_field.
  CLEAR ls_field.

  ls_field-col_pos   = 4.
  ls_field-fieldname = 'TYPE'.
  ls_field-seltext_m = 'Tipo'.
  ls_field-outputlen = 30.
  APPEND ls_field TO lt_field.
  CLEAR ls_field.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title               = 'Log de verificação'
      i_allow_no_selection  = 'X'
      i_screen_start_column = 10
      i_screen_start_line   = 5
      i_screen_end_column   = 150
      i_screen_end_line     = 25
      i_tabname             = 'T_STATUS'
      it_fieldcat           = lt_field
      i_callback_program    = sy-cprog
    TABLES
      t_outtab              = t_status
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.
FORM fm_criar_loc_inst.

  DATA: l_funcloc TYPE bapi_itob_parms-funcloc.

  DATA: l_funcloc_new TYPE bapi_itob_parms-funcloc_int.

  DATA: ls_data_general    TYPE bapi_itob,
        ls_data_especifico TYPE bapi_itob_fl_only,
        ls_return          TYPE bapiret2.

  CASE p_tipo.
    WHEN '0001'.
      LOOP AT git_saida_local_inst_001 INTO DATA(ls_saida_local_inst_001).
        DATA(l_tabix) = sy-tabix.

        CLEAR: ls_data_general, ls_data_especifico, l_funcloc, l_funcloc_new, ls_return.

        l_funcloc = ls_saida_local_inst_001-funcloc.

        MOVE-CORRESPONDING: ls_saida_local_inst_001 TO ls_data_general,
                            ls_saida_local_inst_001 TO ls_data_especifico.

        ls_data_general-asset_no   = |{ ls_data_general-asset_no   ALPHA = IN }|.
        ls_data_general-sub_number = |{ ls_data_general-sub_number ALPHA = IN }|.
        ls_data_general-settlorder = |{ ls_data_general-settlorder ALPHA = IN }|.
        ls_data_general-standorder = |{ ls_data_general-standorder ALPHA = IN }|.
        ls_data_general-costcenter = |{ ls_data_general-costcenter ALPHA = IN }|.
        ls_data_general-constmonth = |{ ls_data_general-constmonth ALPHA = IN }|.
        ls_data_general-catprofile = |{ ls_data_general-catprofile ALPHA = IN }|.
        ls_data_general-authgrp    = |{ ls_data_general-authgrp ALPHA = IN }|.
        ls_data_general-plsectn    = |{ ls_data_general-plsectn ALPHA = IN }|.
        ls_data_general-comp_code  = |{ ls_data_general-comp_code ALPHA = IN }|.
        ls_data_general-consttype  = |{ ls_data_general-consttype ALPHA = IN }|.

        ls_data_especifico-eqsingle = 'X'.

        ls_data_general-descript   = ls_saida_local_inst_001-pltxt.
        ls_data_general-start_from = ls_saida_local_inst_001-acqdate.
        ls_data_general-acqdate    = ls_saida_local_inst_001-ansdt.

        SELECT SINGLE objid
          FROM crhd
          INTO ls_data_general-work_ctr
          WHERE objty = 'A'
          AND   arbpl = ls_saida_local_inst_001-gewrk
          AND   werks = ls_saida_local_inst_001-wergw.

        CALL FUNCTION 'BAPI_FUNCLOC_CREATE' "#EC CI_USAGE_OK[2438131]
          EXPORTING
            external_number = l_funcloc
            data_general    = ls_data_general
            data_specific   = ls_data_especifico
          IMPORTING
            functlocation   = l_funcloc_new
            return          = ls_return.
        IF l_funcloc_new IS NOT INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          CONCATENATE 'Local de instalação' l_funcloc_new 'criado com sucesso' INTO ls_return-message SEPARATED BY space.
          APPEND VALUE #( id = l_tabix status = '@08@' type = 'S' desc_status = ls_return-message ) TO t_status.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = ls_return-message ) TO t_status.
        ENDIF.
      ENDLOOP.
    WHEN '0002'.
      LOOP AT git_saida_local_inst_002 INTO DATA(ls_saida_local_inst_002).
        l_tabix = sy-tabix.

        CLEAR: ls_data_general, ls_data_especifico, l_funcloc, l_funcloc_new, ls_return.

        l_funcloc = ls_saida_local_inst_002-funcloc.

        MOVE-CORRESPONDING: ls_saida_local_inst_002 TO ls_data_general,
                            ls_saida_local_inst_002 TO ls_data_especifico.

        ls_data_general-asset_no   = |{ ls_data_general-asset_no   ALPHA = IN }|.
        ls_data_general-sub_number = |{ ls_data_general-sub_number ALPHA = IN }|.
        ls_data_general-settlorder = |{ ls_data_general-settlorder ALPHA = IN }|.
        ls_data_general-standorder = |{ ls_data_general-standorder ALPHA = IN }|.
        ls_data_general-costcenter = |{ ls_data_general-costcenter ALPHA = IN }|.
        ls_data_general-constmonth = |{ ls_data_general-constmonth ALPHA = IN }|.
        ls_data_general-catprofile = |{ ls_data_general-catprofile ALPHA = IN }|.
        ls_data_general-authgrp    = |{ ls_data_general-authgrp ALPHA = IN }|.
        ls_data_general-plsectn    = |{ ls_data_general-plsectn ALPHA = IN }|.
        ls_data_general-comp_code  = |{ ls_data_general-comp_code ALPHA = IN }|.
*---> 07/07/2023 - Migração S4 - RZ
*        ls_data_general-consttype  = |{ ls_data_general-consttype ALPHA = IN }|.

        DATA(v_len) = strlen( ls_data_general-consttype ).

        IF v_len > 18.
          ls_data_general-consttype_long  =   ls_data_general-consttype.
        ELSE.
          ls_data_general-consttype  = |{ ls_data_general-consttype ALPHA = IN }|.
        ENDIF.
*<--- 07/07/2023 - Migração S4 - RZ


        ls_data_especifico-eqsingle = 'X'.

        ls_data_general-descript   = ls_saida_local_inst_002-pltxt.
        ls_data_general-start_from = ls_saida_local_inst_002-acqdate.
        ls_data_general-acqdate    = ls_saida_local_inst_002-ansdt.

        SELECT SINGLE objid
          FROM crhd
          INTO ls_data_general-work_ctr
          WHERE objty = 'A'
          AND   arbpl = ls_saida_local_inst_002-gewrk
          AND   werks = ls_saida_local_inst_002-wergw.

        CALL FUNCTION 'BAPI_FUNCLOC_CREATE' "#EC CI_USAGE_OK[2438131]
          EXPORTING
            external_number = l_funcloc
            data_general    = ls_data_general
            data_specific   = ls_data_especifico
          IMPORTING
            functlocation   = l_funcloc_new
            return          = ls_return.
        IF l_funcloc_new IS NOT INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          CONCATENATE 'Local de instalação' l_funcloc_new 'criado com sucesso' INTO ls_return-message SEPARATED BY space.
          APPEND VALUE #( id = l_tabix status = '@08@' type = 'S' desc_status = ls_return-message ) TO t_status.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = ls_return-message ) TO t_status.
        ENDIF.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

  IF t_status[] IS NOT INITIAL.
    PERFORM f_alv_log.
  ENDIF.

ENDFORM.
FORM fm_modificar_loc_inst.

  DATA: functlocation  LIKE  bapi_itob_parms-funcloc_int,
        data_general   LIKE  bapi_itob,
        data_generalx  LIKE  bapi_itobx,
        data_specific  LIKE  bapi_itob_fl_only,
        data_specificx LIKE  bapi_itob_fl_onlyx,
        ls_return      TYPE bapiret2.

  CASE p_tipo.
    WHEN '0001'.
      LOOP AT git_saida_local_inst_001 INTO DATA(ls_saida_local_inst_001).
        DATA(l_tabix) = sy-tabix.

        CLEAR: functlocation, data_general, data_specific, ls_return, data_generalx, data_specificx.

        functlocation = ls_saida_local_inst_001-funcloc.

        MOVE-CORRESPONDING: ls_saida_local_inst_001 TO data_general,
                            ls_saida_local_inst_001 TO data_specific.

        data_general-asset_no   = |{ data_general-asset_no   ALPHA = IN }|.
        data_general-sub_number = |{ data_general-sub_number ALPHA = IN }|.
        data_general-settlorder = |{ data_general-settlorder ALPHA = IN }|.
        data_general-standorder = |{ data_general-standorder ALPHA = IN }|.
        data_general-costcenter = |{ data_general-costcenter ALPHA = IN }|.
        data_general-constmonth = |{ data_general-constmonth ALPHA = IN }|.
        data_general-catprofile = |{ data_general-catprofile ALPHA = IN }|.
        data_general-authgrp    = |{ data_general-authgrp ALPHA = IN }|.
        data_general-comp_code  = |{ data_general-comp_code ALPHA = IN }|.
        data_general-plsectn    = |{ data_general-plsectn ALPHA = IN }|.
* ---> S4 Migration - 07/07/2023 - RZ - Inicio
*        data_general-consttype  = |{ data_general-consttype ALPHA = IN }|.

        DATA(v_len) = strlen( data_general-consttype ).

        IF v_len > 18.
          data_general-consttype_long  =   data_general-consttype.
        ELSE.
          data_general-consttype  = |{ data_general-consttype ALPHA = IN }|.
        ENDIF.
* <--- S4 Migration - 07/07/2023 - RZ - Fim



        data_specific-eqsingle = 'X'.

        data_general-descript   = ls_saida_local_inst_001-pltxt.
        data_general-start_from = ls_saida_local_inst_001-acqdate.
        data_general-acqdate    = ls_saida_local_inst_001-ansdt.

        SELECT SINGLE objid
          FROM crhd
          INTO data_general-work_ctr
          WHERE objty = 'A'
          AND   arbpl = ls_saida_local_inst_001-gewrk
          AND   werks = ls_saida_local_inst_001-wergw.

        DO.
          ASSIGN COMPONENT sy-index OF STRUCTURE data_general TO FIELD-SYMBOL(<value>).
          IF sy-subrc IS INITIAL.
            IF <value> IS NOT INITIAL.
              ASSIGN COMPONENT sy-index OF STRUCTURE data_generalx TO FIELD-SYMBOL(<mark_value>).
              IF <mark_value> IS ASSIGNED.
                <mark_value> = 'X'.
              ENDIF.
            ENDIF.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.

        DO.
          ASSIGN COMPONENT sy-index OF STRUCTURE data_specific TO <value>.
          IF sy-subrc IS INITIAL.
            IF <value> IS NOT INITIAL.
              ASSIGN COMPONENT sy-index OF STRUCTURE data_specificx TO <mark_value>.
              IF <mark_value> IS ASSIGNED.
                <mark_value> = 'X'.
              ENDIF.
            ENDIF.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.

        "Tratativa para campos de material realizada. Pseudo comentário adicionado   " >> ---> S4 Migration - 07/07/2023 - RZ
        CALL FUNCTION 'BAPI_FUNCLOC_CHANGE'            "#EC CI_USAGE_OK[2438131]   " >> ---> S4 Migration - 07/07/2023 - RZ
          EXPORTING
            functlocation  = functlocation
            data_general   = data_general
            data_generalx  = data_generalx
            data_specific  = data_specific
            data_specificx = data_specificx
          IMPORTING
            return         = ls_return.
        IF ls_return-type EQ 'E'.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = ls_return-message ) TO t_status.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          CONCATENATE 'Local de instalação' functlocation 'modificado com sucesso' INTO ls_return-message SEPARATED BY space.
          APPEND VALUE #( id = l_tabix status = '@08@' type = 'S' desc_status = ls_return-message ) TO t_status.
        ENDIF.
      ENDLOOP.
    WHEN '0002'.
      LOOP AT git_saida_local_inst_002 INTO DATA(ls_saida_local_inst_002).
        l_tabix = sy-tabix.

        CLEAR: functlocation, data_general, data_specific, ls_return, data_generalx, data_specificx.

        functlocation = ls_saida_local_inst_002-funcloc.

        MOVE-CORRESPONDING: ls_saida_local_inst_002 TO data_general,
                            ls_saida_local_inst_002 TO data_specific.

        data_specific-eqsingle = 'X'.


        data_general-descript   = ls_saida_local_inst_002-pltxt.
        data_general-start_from = ls_saida_local_inst_002-acqdate.
        data_general-acqdate    = ls_saida_local_inst_002-ansdt.

        SELECT SINGLE objid
          FROM crhd
          INTO data_general-work_ctr
          WHERE objty = 'A'
          AND   arbpl = ls_saida_local_inst_002-gewrk
          AND   werks = ls_saida_local_inst_002-wergw.

        data_general-asset_no   = |{ data_general-asset_no   ALPHA = IN }|.
        data_general-sub_number = |{ data_general-sub_number ALPHA = IN }|.
        data_general-settlorder = |{ data_general-settlorder ALPHA = IN }|.
        data_general-standorder = |{ data_general-standorder ALPHA = IN }|.
        data_general-costcenter = |{ data_general-costcenter ALPHA = IN }|.
        data_general-constmonth = |{ data_general-constmonth ALPHA = IN }|.
        data_general-catprofile = |{ data_general-catprofile ALPHA = IN }|.
        data_general-authgrp    = |{ data_general-authgrp ALPHA = IN }|.
        data_general-plsectn    = |{ data_general-plsectn ALPHA = IN }|.
        data_general-comp_code  = |{ data_general-comp_code ALPHA = IN }|.
        data_general-consttype  = |{ data_general-consttype ALPHA = IN }|.

        DO.
          ASSIGN COMPONENT sy-index OF STRUCTURE data_general TO <value>.
          IF sy-subrc IS INITIAL.
            IF <value> IS NOT INITIAL.
              ASSIGN COMPONENT sy-index OF STRUCTURE data_generalx TO <mark_value>.
              IF <mark_value> IS ASSIGNED.
                <mark_value> = 'X'.
              ENDIF.
            ENDIF.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.

        DO.
          ASSIGN COMPONENT sy-index OF STRUCTURE data_specific TO <value>.
          IF sy-subrc IS INITIAL.
            IF <value> IS NOT INITIAL.
              ASSIGN COMPONENT sy-index OF STRUCTURE data_specificx TO <mark_value>.
              IF <mark_value> IS ASSIGNED.
                <mark_value> = 'X'.
              ENDIF.
            ENDIF.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.

        CALL FUNCTION 'BAPI_FUNCLOC_CHANGE' "#EC CI_USAGE_OK[2438131]
          EXPORTING
            functlocation  = functlocation
            data_general   = data_general
            data_generalx  = data_generalx
            data_specific  = data_specific
            data_specificx = data_specificx
          IMPORTING
            return         = ls_return.
        IF ls_return-type EQ 'E'.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          APPEND VALUE #( id = l_tabix status = '@0A@' type = 'E' desc_status = ls_return-message ) TO t_status.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          CONCATENATE 'Local de instalação' functlocation 'modificado com sucesso' INTO ls_return-message SEPARATED BY space.
          APPEND VALUE #( id = l_tabix status = '@08@' type = 'S' desc_status = ls_return-message ) TO t_status.
        ENDIF.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

  IF t_status[] IS NOT INITIAL.
    PERFORM f_alv_log.
  ENDIF.

ENDFORM.


FORM f_importar_anexos .

  FREE: t_anexos.
  CLEAR: l_obj_key, l_ip_service, w_bor, l_ip_service.

  l_obj_key = 'ZPM0085'.

  CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
    EXPORTING
      classname          = 'ZPM0085_ANEXO'
      objkey             = l_obj_key
      client             = sy-mandt
    TABLES
      gos_connections    = t_anexos
    EXCEPTIONS
      no_objects_found   = 1
      internal_error     = 2
      internal_gos_error = 3
      OTHERS             = 4.

  DESCRIBE TABLE t_anexos LINES l_lines.

  CREATE OBJECT anexo_obj TYPE cl_gos_manager.

  l_ip_mode     = 'E'.
  l_ip_service  = COND #( WHEN l_lines = 0 THEN 'PCATTA_CREA'
                                           ELSE 'VIEW_ATTA' ).
  w_bor-objkey  = l_obj_key. "l_chave.
  w_bor-objtype = 'ZPM0085_ANEX'.

  anexo_obj->set_rw_mode( ip_mode = l_ip_mode ).

  anexo_obj->start_service_direct(
    EXPORTING
      ip_service         = l_ip_service
      is_object          = w_bor
    EXCEPTIONS
      no_object          = 1
      object_invalid     = 2
      execution_failed   = 3
      OTHERS             = 4 ).

  WAIT UP TO 2 SECONDS.

  COMMIT WORK.

ENDFORM.
