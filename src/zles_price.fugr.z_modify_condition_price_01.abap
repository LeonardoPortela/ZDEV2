FUNCTION z_modify_condition_price_01.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_VTTP STRUCTURE  VTTP
*"      C_KOMV STRUCTURE  KOMV_INDEX
*"      T_VFSIVB STRUCTURE  VFSIVB OPTIONAL
*"----------------------------------------------------------------------

  FIELD-SYMBOLS: <field> TYPE any.

  "Internal Table e Work Area
  DATA: wa_vttp     TYPE vttp,
        ti_lips     TYPE TABLE OF lips,
        ti_vtpa     TYPE TABLE OF vtpa,
        wa_lips     TYPE lips,
        wa_vttk     TYPE vttk,
        wa_vttk_aux TYPE vttk.

  DATA: v_vbeln         TYPE  vbeln,
        v_placa_cav     TYPE  zplaca,
        v_vlr_frete_neg TYPE  zvalor_frete,
        v_lifnr_pc      TYPE  vtpa-lifnr,
        v_lifnr_sp      TYPE  vtpa-lifnr.


  CLEAR: wa_vttk, wa_vttk_aux, ti_vtpa[], ti_lips[].

  READ TABLE t_vttp INTO wa_vttp INDEX 1.

  CHECK sy-subrc = 0.

  SELECT SINGLE *
    FROM vttk INTO wa_vttk
   WHERE tknum EQ wa_vttp-tknum.

  CHECK sy-subrc = 0.

  SELECT *
    FROM vtpa INTO TABLE ti_vtpa
   WHERE vbeln EQ wa_vttp-tknum.

  ASSIGN ('(SAPMV56A)VTTK') TO <field>.

  IF <field> IS ASSIGNED.
    MOVE-CORRESPONDING: <field> TO wa_vttk_aux .
  ENDIF.

  IF wa_vttk-shtyp IS INITIAL.
    MOVE wa_vttk_aux-shtyp TO wa_vttk-shtyp.
  ENDIF.

  IF wa_vttk-tdlnr IS INITIAL.
    MOVE wa_vttk_aux-tdlnr TO wa_vttk-tdlnr.
  ENDIF.

  IF wa_vttk-route IS INITIAL.
    MOVE wa_vttk_aux-route TO wa_vttk-route.
  ENDIF.

  IF wa_vttk-sdabw IS INITIAL.
    MOVE wa_vttk_aux-sdabw TO wa_vttk-sdabw.
  ENDIF.

  SELECT *
    FROM lips INTO TABLE ti_lips
     FOR ALL ENTRIES IN t_vttp
   WHERE vbeln EQ t_vttp-vbeln.

  CHECK ti_lips[] IS NOT INITIAL.

  DATA(lva_achou_lips) = abap_false.
  IF t_vfsivb[] IS NOT INITIAL AND c_komv-kposn IS NOT INITIAL.
    READ TABLE t_vfsivb INTO DATA(lwa_vfsi) WITH KEY kposn = c_komv-kposn.
    IF sy-subrc EQ 0 AND lwa_vfsi-vbeln IS NOT INITIAL AND lwa_vfsi-posnr IS NOT INITIAL.
      READ TABLE ti_lips INTO wa_lips WITH KEY vbeln = lwa_vfsi-vbeln
                                               posnr = lwa_vfsi-posnr.
      IF sy-subrc EQ 0.
        lva_achou_lips = abap_true.
      ENDIF.
    ENDIF.
  ENDIF.

  IF lva_achou_lips EQ abap_false.
    READ TABLE ti_lips INTO wa_lips INDEX 1.
  ENDIF.

*------------------------------------------------------------------------------------------------------*
* Ajustes Condicoes ZPTA ZIPT ZICC
*------------------------------------------------------------------------------------------------------*

  PERFORM f_adjustements_price_pauta TABLES c_komv
                                            ti_vtpa
                                      USING wa_vttk
                                            wa_lips.

*------------------------------------------------------------------------------------------------------*
* Ajustes Condicoes ZFRE
*------------------------------------------------------------------------------------------------------*

  PERFORM f_adjustements_price_zfre TABLES c_komv
                                           ti_vtpa
                                     USING wa_vttk
                                           wa_lips.
*------------------------------------------------------------------------------------------------------*
*  Ajustes Condicoes ZISR e ZIRF (PJ)
*------------------------------------------------------------------------------------------------------*
  PERFORM f_adjustements_price_zisr_zirf TABLES c_komv
                                                ti_vtpa
                                          USING wa_vttk
                                                wa_lips.


*------------------------------------------------------------------------------------------------------*
*  Ajustes Condicoes ZBIR
*------------------------------------------------------------------------------------------------------*
*  PERFORM f_adjustements_price_zbir TABLES c_komv
*                                           ti_vtpa
*                                     USING wa_vttk
*                                           wa_lips.


*------------------------------------------------------------------------------------------------------*
* Ajustes Outras Condicoes abaixo...
*------------------------------------------------------------------------------------------------------*




ENDFUNCTION.
