*&---------------------------------------------------------------------*
*&  Include           ZLESR0162_TOP
*&---------------------------------------------------------------------*

TYPES:
  BEGIN OF ty_kna1,
    kunnr     TYPE kna1-kunnr,
    name1     TYPE kna1-name1,
    land1     TYPE kna1-land1,
    stras     TYPE kna1-stras,
    ort01     TYPE kna1-ort01,
    regio     TYPE kna1-regio,
    stcd1     TYPE kna1-stcd1,
    stcd3     TYPE kna1-stcd3,
    lzone     TYPE kna1-lzone,
    zone_desc TYPE tzont-vtext,
**<<<------"187200 - NMS - INI------>>>
    zlatitude  TYPE tzone-zlatitude,
    zlongitude TYPE tzone-zlongitude,
**<<<------"187200 - NMS - FIM------>>>
  END OF ty_kna1,

  BEGIN OF ty_trolz,
    aland TYPE trolz-aland,
    azone TYPE trolz-azone,
    lland TYPE trolz-lland,
    lzone TYPE trolz-lzone,
    route TYPE trolz-route,
    desc  TYPE char40,
  END OF ty_trolz,

  BEGIN OF ty_lfa1,
    lifnr     TYPE lfa1-lifnr,
    name1     TYPE lfa1-name1,
    land1     TYPE lfa1-land1,
    stras     TYPE lfa1-stras,
    ort01     TYPE lfa1-ort01,
    regio     TYPE lfa1-regio,
    stcd1     TYPE lfa1-stcd1,
    stcd3     TYPE lfa1-stcd3,
    lzone     TYPE lfa1-lzone,
    zone_desc TYPE tzont-vtext,
**<<<------"187200 - NMS - INI------>>>
    zlatitude  TYPE tzone-zlatitude,
    zlongitude TYPE tzone-zlongitude,
**<<<------"187200 - NMS - FIM------>>>
  END OF ty_lfa1.

DATA: wa_kna1    TYPE ty_kna1,
      wa_trolz   TYPE ty_trolz,
      wa_lfa1_pc TYPE ty_lfa1.

DATA: v_distancia  TYPE distz,
      v_km         TYPE char2 VALUE 'KM',
      v_tp_exped   TYPE char2 VALUE '01',
      v_cal_fab    TYPE char2 VALUE 'BR',
      v_chk_relev  TYPE flag  VALUE 'X',
      v_ck_pedagio TYPE zde_ck_pegagio,  "*-CS2024000877-06.12.2024-#154505-JT
      v_dur_tran   TYPE p LENGTH 6 DECIMALS 2,
      v_lifnr      TYPE lfa1-lifnr,
      v_kunnr      TYPE kna1-kunnr,
      lv_zone_ori  TYPE trolz-lzone,
      lv_zone_des  TYPE trolz-lzone,
      gt_bdcdata   TYPE TABLE OF bdcdata.

TYPES: BEGIN OF ty_simnao, "170657 CS2025000299 CHECKBOX TO LIST ZLES0214 PSA
         name TYPE char3,
         id   TYPE char1,
       END OF ty_simnao.

DATA: list          TYPE vrm_values, "170657 CS2025000299 CHECKBOX TO LIST ZLES0214 PSA
      value         LIKE LINE OF list, "170657 CS2025000299 CHECKBOX TO LIST ZLES0214 PSA
      it_f1_sim_nao TYPE STANDARD TABLE OF ty_simnao INITIAL SIZE 0,"170657 CS2025000299 CHECKBOX TO LIST ZLES0214 PSA
      p_pedagio     TYPE char3."170657 CS2025000299 CHECKBOX TO LIST ZLES0214 PSA
