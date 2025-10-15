class ZCL_IM_LE_SHP_GOODSMOVEMEN definition
  public
  final
  create public .

*"* public components of class ZCL_IM_LE_SHP_GOODSMOVEMEN
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_LE_SHP_GOODSMOVEMENT .
protected section.
*"* protected components of class ZCL_IM_LE_SHP_GOODSMOVEMEN
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_LE_SHP_GOODSMOVEMEN
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_LE_SHP_GOODSMOVEMEN IMPLEMENTATION.


METHOD if_ex_le_shp_goodsmovement~change_input_header_and_items.

  DATA: vl_adrnr_kn TYPE kna1-adrnr     ,
        vl_adrnr_lf TYPE kna1-adrnr     ,
        vl_trans_kn TYPE adrc-transpzone,
        vl_trans_lf TYPE adrc-transpzone,
        vl_route    TYPE trolz-route    ,
        vl_campo    TYPE char30         ,
        sl_vbpa     TYPE vbpavb         .

  FIELD-SYMBOLS <route> TYPE likp-route.

  IF is_likp-lfart EQ 'TFDV'  AND
     if_tcode      EQ 'VL02N' AND
     if_fcode      EQ 'WABU_T'.

    READ TABLE it_xvbpa INTO sl_vbpa
      WITH KEY parvw = 'PC'.
    IF NOT sy-subrc IS INITIAL.
      MESSAGE e836(sd) WITH 'Preencher parceiro PC – Ponto de Coleta'.
    ENDIF.

    SELECT SINGLE adrnr
      FROM kna1
      INTO vl_adrnr_kn
    WHERE  kunnr EQ is_likp-kunnr.
    SELECT SINGLE adrnr
      FROM lfa1
      INTO vl_adrnr_lf
    WHERE  lifnr EQ sl_vbpa-lifnr.
    IF NOT vl_adrnr_kn IS INITIAL.
      SELECT SINGLE transpzone
        FROM adrc
        INTO vl_trans_kn
      WHERE  addrnumber EQ vl_adrnr_kn.
    ENDIF.
    IF NOT vl_adrnr_lf IS INITIAL.
      SELECT SINGLE transpzone
        FROM adrc
        INTO vl_trans_lf
      WHERE  addrnumber EQ vl_adrnr_lf.
    ENDIF.
    IF NOT vl_trans_kn IS INITIAL AND
       NOT vl_trans_lf IS INITIAL.
      SELECT SINGLE route
        FROM trolz
        INTO vl_route
      WHERE  aland EQ 'BR'
        AND  azone EQ vl_trans_lf
        AND  lland EQ 'BR'
        AND  lzone EQ vl_trans_kn.
      IF NOT vl_route IS INITIAL.
        vl_campo = '(SAPFV50W)LIKP-ROUTE'.
        ASSIGN (vl_campo) TO <route>.
        IF <route> IS ASSIGNED.
          <route> = vl_route.
          MESSAGE i836(sd) WITH 'O Itinerário foi Redeterminado.'.
        ENDIF.
      ELSE.
        MESSAGE i836(sd) WITH 'O Itinerário não foi Redeterminado.'
                              'verifique zonas de transporte'.
      ENDIF.
    ELSE.
      MESSAGE i836(sd) WITH 'O Itinerário não foi Redeterminado.'
                            'verifique zonas de transporte'.
    ENDIF.

  ENDIF.

ENDMETHOD.
ENDCLASS.
