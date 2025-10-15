"Name: \PR:SAPLV60A\FO:USEREXIT_PRICING_PREPARE_TKOMK\SE:BEGIN\EI
ENHANCEMENT 0 ZSD_VF01_RV60AFZZ.

************************************************************************************************

TYPES: BEGIN OF ty_insert,
         doc   TYPE zsdt0041-doc_simulacao,
         vbeln TYPE vbeln,
         vbelv TYPE vbelv,
         matnr TYPE matnr,
         posnr TYPE posnr,
         charg TYPE charg_d,
       END OF ty_insert,

       BEGIN OF ty_0090.
         INCLUDE STRUCTURE zsdt0090.
TYPES posnr TYPE vbap-posnr.
TYPES END OF ty_0090.

DATA: it_insert TYPE TABLE OF ty_insert,
      wa_insert TYPE ty_insert.
DATA: im_0090 TYPE zsdt0090,
      p_0090  TYPE ty_0090.

DATA: wa_setleaf TYPE setleaf.

DATA: lt_vbfa TYPE TABLE OF vbfa,
      lw_vbfa TYPE vbfa.

DATA: lt_vbfa_equ TYPE TABLE OF vbfa,
      lw_vbfa_equ TYPE vbfa.

DATA: lt_zsdt0053 TYPE TABLE OF zsdt0053,
      lw_zsdt0053 TYPE zsdt0053.

DATA: lt_zsdt0055 TYPE TABLE OF zsdt0055,
      lw_zsdt0055 TYPE zsdt0055.

DATA: lt_zsdt0073 TYPE TABLE OF zsdt0073,
      lw_zsdt0073 TYPE zsdt0073.

DATA: wa_ins_0053      TYPE zsdt0053.
DATA: lt_zsdt0053_item TYPE TABLE OF zsdt0053,
      lw_zsdt0053_item TYPE zsdt0053,

      lt_zsdt0053_aux  TYPE TABLE OF zsdt0053,
      lw_zsdt0053_aux  TYPE zsdt0053,

      lt_zsdt0041      TYPE TABLE OF zsdt0041,
      lw_zsdt0041      TYPE zsdt0041,

      lt_zsdt0090      TYPE TABLE OF zsdt0090,
      lw_zsdt0090      TYPE zsdt0090,

      lt_zsdt0053_equ  TYPE TABLE OF zsdt0053,
      lw_zsdt0053_equ  TYPE zsdt0053,

      lt_vbap          TYPE TABLE OF vbap,
      lw_vbap          TYPE vbap.

DATA: var_next_posnr TYPE zsdt0053-posnr,
      var_tabix      TYPE sy-tabix.

DATA: lt_zsdt0059 TYPE TABLE OF zsdt0059,
      lw_zsdt0059 TYPE zsdt0059.

DATA: lt_zsdt0100 TYPE TABLE OF zsdt0100,
      lw_zsdt0100 TYPE zsdt0100.

DATA: lt_zsdt0056 TYPE TABLE OF zsdt0056,
      lw_zsdt0056 TYPE zsdt0056.

DATA: r_bezei      TYPE RANGE OF zsdt0056-bezei,
      r_bezei_line LIKE LINE OF r_bezei.

DATA: c_bezei      TYPE RANGE OF zsdt0056-bezei,
      c_bezei_line LIKE LINE OF c_bezei.

DATA: p_bezei      TYPE RANGE OF zsdt0056-bezei,
      p_bezei_line LIKE LINE OF p_bezei.

DATA: var_len   TYPE i.
DATA: var_dir   TYPE bezei30.
DATA: dir_mi_in   TYPE c LENGTH 3.

DATA: var_bezei TYPE zsdt0056-bezei.

DATA: var_formula  TYPE zsdt0059-formula.

DATA: var_msg                       TYPE string.
DATA: cx_exception                  TYPE REF TO zcx_webservice.
DATA: obj_zcl_webservice_taxa_curva TYPE REF TO zcl_webservice_tx_curva.

DATA(obj_auart) = NEW zcl_taxa_curva( ).

DATA: r_auart TYPE RANGE OF auart.
DATA: r_comp TYPE RANGE OF auart.
DATA: r_devo_recu TYPE RANGE OF auart.
DATA: r_auart_dev TYPE RANGE OF auart.   "<<RIM-SKM-CS1025614-16.09.22

DATA: r_auart_ztrg TYPE RANGE OF auart.

DATA: var_transf(20) TYPE c,
      msg            TYPE string.

DATA: sl_header          TYPE bapi2017_gm_head_01,
      vl_code            TYPE bapi2017_gm_code,
      vl_doc             TYPE bapi2017_gm_head_ret-mat_doc,
      vl_year            TYPE bapi2017_gm_head_ret-doc_year,
      tl_item            TYPE TABLE OF bapi2017_gm_item_create,
      tl_return          TYPE TABLE OF bapiret2,
      sl_return          TYPE bapiret2,
      sl_item            TYPE bapi2017_gm_item_create,
      it_return          TYPE TABLE OF bapiret2,
      wa_return          TYPE bapiret2,
      es_bflushflags     LIKE bapi_rm_flg,
      es_bflushdatagen   LIKE bapi_rm_datgen,
      es_confirmation    LIKE bapi_rm_datkey-confirmation,
      sperr_user         TYPE sy-msgv1,
      fg_bloqueio(1),
      vlines             TYPE sy-tabix,
      vg_interface(2),
      lva_msg_erro_vf    TYPE string,
      lva_valor_s_aux_01 TYPE c LENGTH 50,
      lva_valor_s_aux_02 TYPE c LENGTH 50,
      vg_obj_key         TYPE zmmt_ee_zgr-obj_key,
      it_outreturn       TYPE TABLE OF zfie_ret_document,
      wa_outreturn       TYPE zfie_ret_document,
      it_zppt0006        TYPE TABLE OF zppt0006,
      wa_zppt0006        TYPE zppt0006,
      wa_head_ret        TYPE bapi2017_gm_head_ret.

DATA: obj TYPE REF TO zcl_solicitacao_ov.
DATA vcategoria TYPE c.

r_comp = obj_auart->get_auart( 'ZHEDGECOMP' ). " Get SET de AUART de Complemento
r_devo_recu = obj_auart->get_auart( 'ZHEDGEDEVO/RECU' ). " Get SET de AUART de Devolução/Recusa
r_auart = obj_auart->get_auart( 'TODOS' ). " Get SET de AUART de Devolução/Recusa
r_auart_dev = obj_auart->get_auart( 'ZVF01_AUART_DEV' ). " Dev para mov 411 <<RIM-SKM-CS1025614-16.09.22


CREATE OBJECT obj.

SELECT SINGLE *
  FROM setleaf
  INTO wa_setleaf
 WHERE setname = 'MAGGI_EMPRESA_EXTERIOR'
   AND valfrom = tkomk-vkorg.

IF ( sy-subrc NE 0 ).
  IF sy-tcode = 'VF01'.

    CLEAR: wa_setleaf.
    SELECT SINGLE *
      FROM setleaf
      INTO wa_setleaf
     WHERE setname = 'VF01_USUARIO'
       AND valfrom = sy-uname.

    IF NOT ( tkomk-fkdat IS INITIAL ) AND ( tkomk-fkdat NE sy-datum ) AND ( sy-subrc IS NOT INITIAL ).

      "Set utilizado por odens ZTRG criadas pela transação ZSDT0158
      SELECT *
            FROM setleaf
            INTO TABLE @DATA(it_set_ztrg)
           WHERE setname = 'MAGGI_VF01_TP_OV'.

      LOOP AT it_set_ztrg INTO DATA(wa_set_ztrg).
        APPEND VALUE rsdsselopt( option = wa_set_ztrg-valoption
                                 sign   = wa_set_ztrg-valsign
                                 low    = wa_set_ztrg-valfrom )
                            TO r_auart_ztrg.
      ENDLOOP.

      IF tkomk-auart_sd NOT IN r_auart_ztrg.

        MESSAGE e000(z01) WITH 'A data do faturamento tem que ser ' sy-datum.
        STOP.

      ENDIF.

    ENDIF.

  ENDIF.
ENDIF.

*-CS2023000060-08.02.2023-#101882-JT-inicio
INCLUDE zsdr0149.
*-CS2023000060-08.02.2023-#101882-JT-fim

IF tkomk-auart_sd IN r_devo_recu.
  var_dir = 'Y'.
ELSEIF tkomk-auart_sd IN r_comp.
  var_dir = 'W'.
ENDIF.

SELECT SINGLE *
  FROM mara
  INTO @DATA(wa_mara)
WHERE matnr EQ @vbrp-matnr.

CASE sy-tcode.
  WHEN: 'VF01'.

    SELECT SINGLE *
      FROM zfiwrt0009 AS a INTO @DATA(lwa_zfiwrt0009)
     WHERE vbeln EQ @vbap-vbeln
       AND EXISTS ( SELECT seq_lcto
                      FROM zfiwrt0008 AS b
                     WHERE b~seq_lcto        EQ a~seq_lcto
                       AND b~docs_estornados EQ @abap_false
                       AND b~loekz           EQ @abap_false ).

    IF ( sy-subrc EQ 0 ) AND ( vbap-vbeln IS NOT INITIAL ).
      CONCATENATE 'Ordem:' vbap-vbeln 'vinculada ao Lançamento da ZNFW Seq.Lcto:' lwa_zfiwrt0009-seq_lcto '!' INTO DATA(lva_msg_erro) SEPARATED BY space.
      MESSAGE lva_msg_erro TYPE 'E'.
    ENDIF.

    IF wa_mara-mtart = 'ZFER'.
      IF tkomk-auart_sd IN r_auart_dev.    "<<RIM-SKM-CS1025614-15.09.22
*        IF TKOMK-AUART_SD = 'ZROB' OR TKOMK-AUART_SD = 'ZREB' OR  TKOMK-AUART_SD = 'ZRPF'. ">>RIM-SKM-CS1025614-15.09.22
        IF tkomk-werks = '0175'.
          SELECT SINGLE * FROM zppt0017 INTO @DATA(wa_zppt0017)
            WHERE situacao EQ 'A'
            AND   werks    EQ @tkomk-werks
            AND   matnr_ac EQ @vbap-matnr.

          IF  wa_zppt0017 IS INITIAL.
            MESSAGE 'Não Existe DE/PARA de material cadastrado na transação ZPP0015!' TYPE 'E'.
            EXIT.
          ENDIF.

          SELECT SINGLE * FROM mard INTO @DATA(wa_mard)
           WHERE matnr EQ @wa_zppt0017-matnr_rv
             AND werks EQ @wa_zppt0017-werks
             AND lgort EQ 'IN01'.

          CLEAR msg.

          IF wa_mard IS INITIAL.
            CONCATENATE 'Material de Revenda' wa_zppt0017-matnr_rv 'não expandido para o Centro' wa_zppt0017-werks 'e Depósito IN01 !' INTO msg SEPARATED BY space.
            MESSAGE msg TYPE 'E'.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.



***    IF TKOMK-AUART_SD IN R_AUART.
***
***      REFRESH LT_VBFA_EQU.
***
***      SELECT * FROM VBFA
***          INTO TABLE LT_VBFA_EQU
***        WHERE VBELN   EQ VBAK-VGBEL
***          AND VBTYP_N EQ 'M'
***          AND VBTYP_V IN ('C', 'L').
***
***      READ TABLE LT_VBFA_EQU INTO LW_VBFA_EQU WITH KEY VBELN = VBAK-VGBEL
***                                                     VBTYP_N = 'M'
***                                                     VBTYP_V = 'C'.
***
***      IF SY-SUBRC IS NOT INITIAL.
****     CASO A VENDA FOR FUTURA
***        READ TABLE LT_VBFA_EQU INTO LW_VBFA_EQU WITH KEY VBELN = VBAK-VGBEL
***                                                       VBTYP_N = 'M'
***                                                       VBTYP_V = 'L'.
***      ENDIF.
***
***      IF LT_VBFA_EQU IS NOT INITIAL.
***
***        SELECT * FROM ZSDT0053
***          INTO TABLE LT_ZSDT0053_EQU
***                FOR ALL ENTRIES IN LT_VBFA_EQU
***                WHERE VBELN EQ LT_VBFA_EQU-VBELV.
***
***        SELECT * FROM ZSDT0051
***          INTO TABLE @DATA(IT_ZSDT0051)
***                FOR  ALL ENTRIES IN @LT_ZSDT0053_EQU
***                WHERE NRO_SOL_OV EQ @LT_ZSDT0053_EQU-NRO_SOL_OV.
***
***        IF  NOT LT_ZSDT0053_EQU IS INITIAL.
***          DATA(_MOEDA) = IT_ZSDT0051[ 1 ]-WAERK. "// Alterado para atender o CS2018001046 wbarbosa
***          DIR_MI_IN = 'MI'.
***        ELSE.
***
***          FREE IT_INSERT.
***
***          SELECT *
***            FROM VBAP
***            INTO TABLE LT_VBAP
***              FOR ALL ENTRIES IN LT_VBFA_EQU
***              WHERE VBELN EQ LT_VBFA_EQU-VBELV
***                AND POSNR EQ LT_VBFA_EQU-POSNV.
***
***          IF NOT LT_VBAP IS INITIAL.
***            SELECT *
***              FROM ZSDT0041
***              INTO TABLE LT_ZSDT0041
***                FOR ALL ENTRIES IN LT_VBAP
***                WHERE VBELN EQ LT_VBAP-VBELN
***                  AND MATNR EQ LT_VBAP-MATNR.
***
***            IF NOT LT_ZSDT0041 IS INITIAL.
***              READ TABLE LT_ZSDT0041 INTO LW_ZSDT0041 WITH KEY VBELN = LW_VBFA_EQU-VBELV.
***
***              LOOP AT AVBAP[] INTO DATA(WA_VBAP).
***
***                WA_INSERT-DOC    = LW_ZSDT0041-DOC_SIMULACAO.
***                WA_INSERT-VBELN  = WA_VBAP-VBELN.
***                WA_INSERT-VBELV  = LW_ZSDT0041-VBELN.
***                WA_INSERT-MATNR  = WA_VBAP-MATNR.
***                WA_INSERT-POSNR  = LW_ZSDT0041-POSNR.
***                WA_INSERT-CHARG  = WA_VBAP-CHARG.
***
***                APPEND WA_INSERT TO IT_INSERT.
***
***              ENDLOOP.
***
***              DIR_MI_IN = 'IN'.
***            ELSE.
***
***
***              SELECT *
***                FROM ZSDT0090
***                INTO TABLE LT_ZSDT0090
***                  FOR ALL ENTRIES IN LT_VBAP
***                  WHERE VBELN EQ LT_VBAP-VBELN
***                    AND MATNR EQ LT_VBAP-MATNR.
***
***              IF NOT LT_ZSDT0090 IS INITIAL.
***                READ TABLE LT_ZSDT0090 INTO LW_ZSDT0090 WITH KEY VBELN = LW_VBFA_EQU-VBELV.
***
***                LOOP AT AVBAP[] INTO WA_VBAP.
***
***                  WA_INSERT-DOC    = LW_ZSDT0090-DOC_SIMULACAO.
***                  WA_INSERT-VBELN  = WA_VBAP-VBELN.
***                  WA_INSERT-VBELV  = LW_ZSDT0090-VBELN.
***                  WA_INSERT-MATNR  = WA_VBAP-MATNR.
***                  WA_INSERT-POSNR  = LW_ZSDT0090-POSNN.
***                  WA_INSERT-CHARG  = WA_VBAP-CHARG.
***
***                  APPEND WA_INSERT TO IT_INSERT.
***                ENDLOOP.
***
***                DIR_MI_IN = 'IN'.
***              ENDIF.
***            ENDIF.
***          ENDIF.
***        ENDIF.
***      ENDIF.
***
***      READ TABLE LT_ZSDT0053_EQU INTO LW_ZSDT0053_EQU WITH KEY VBELN = LW_VBFA_EQU-VBELV.
***      IF SY-SUBRC EQ 0.
***
***        TRY.
***            DATA(TP_VENDA) = IT_ZSDT0051[ NRO_SOL_OV = LW_ZSDT0053_EQU-NRO_SOL_OV ]-TP_VENDA .
***          CATCH CX_SY_ITAB_LINE_NOT_FOUND.
***            CLEAR TP_VENDA.
***        ENDTRY.
***
***        SELECT COUNT(*)
***          FROM SETLEAF
***          WHERE SETNAME EQ 'MAGGI_ZSDT0062_HEDGE'
***            AND VALFROM EQ TP_VENDA.
***
***        CHECK SY-SUBRC IS INITIAL.
***        CHECK DIR_MI_IN EQ 'MI'.
***
***        CALL FUNCTION 'ENQUEUE_EZSDT0051'
***          EXPORTING
***            NRO_SOL_OV     = LW_ZSDT0053_EQU-NRO_SOL_OV
***          EXCEPTIONS
***            FOREIGN_LOCK   = 1
***            SYSTEM_FAILURE = 2
***            OTHERS         = 3.
***
***        IF SY-SUBRC <> 0.
***          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
***                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
***        ENDIF.
***
***        IF ( SY-UCOMM EQ 'SICH' ).
***          IF TKOMK-AUART_SD IN R_AUART. " Complemento e Devolução de Peso e Preço.
***
***            CLEAR VAR_DIR.
***            IF TKOMK-AUART_SD IN R_DEVO_RECU.
***              VBAP-BRGEW = VBAP-BRGEW * -1.
***              VAR_DIR = 'Y'.
***            ELSEIF TKOMK-AUART_SD IN R_COMP.
***              VAR_DIR = 'W'.
***            ENDIF.
***
***            SELECT SINGLE * FROM ZSDT0053 INTO LW_ZSDT0053 WHERE VBELN EQ VBAP-VBELN AND STATUS NE 'C'.
***            IF ( SY-SUBRC NE 0 ).
***
***              IF VAR_DIR EQ 'Y'.
***                SELECT * FROM VBFA
***                  INTO TABLE LT_VBFA
***                    WHERE VBELN   EQ VBAK-VGBEL
***                      AND VBTYP_N EQ 'M'
***                      AND VBTYP_V IN ('C', 'L' ).
***
***              ELSEIF VAR_DIR EQ 'W'.
***                SELECT * FROM VBFA
***                  INTO TABLE LT_VBFA
***                    WHERE VBELN   EQ VBAP-VBELN
***                    AND VBTYP_V EQ 'C'
***                    AND VBTYP_N IN ('C', 'L' ).
***              ENDIF.
***
***              IF ( SY-SUBRC EQ 0 ).
***                SELECT * FROM ZSDT0053
***                  INTO TABLE LT_ZSDT0053
***                  FOR ALL ENTRIES IN LT_VBFA
***                WHERE VBELN EQ LT_VBFA-VBELV.
***
***                IF ( SY-SUBRC EQ 0 ).
***                  READ TABLE LT_ZSDT0053 INTO LW_ZSDT0053 INDEX 1.
***
***                  SELECT * FROM ZSDT0053
***                    INTO TABLE LT_ZSDT0053_ITEM
***                  WHERE NRO_SOL_OV = LW_ZSDT0053-NRO_SOL_OV.
***
***                  SELECT * FROM ZSDT0053
***                    INTO TABLE LT_ZSDT0053_AUX
***                  WHERE NRO_SOL_OV EQ LW_ZSDT0053-NRO_SOL_OV
***                    AND FIXACAO    EQ LW_ZSDT0053-FIXACAO.
***
****                   Venda Simples
***                  IF ( LW_ZSDT0053-FIXACAO IS INITIAL ).
***                    VAR_NEXT_POSNR = LINES( LT_ZSDT0053_ITEM ).
***                    READ TABLE LT_ZSDT0053_ITEM INTO LW_ZSDT0053_ITEM INDEX VAR_NEXT_POSNR.
***                    VAR_NEXT_POSNR = LW_ZSDT0053_ITEM-POSNR + 10.
***                  ELSE.
***                    READ TABLE LT_ZSDT0053_ITEM INTO LW_ZSDT0053_ITEM WITH KEY FIXACAO = LW_ZSDT0053-FIXACAO
***                                                                                STATUS = VAR_DIR
***                                                                                CHARG  = LW_ZSDT0053-CHARG.
***                    IF SY-SUBRC IS INITIAL.
***                      VAR_NEXT_POSNR = LW_ZSDT0053_ITEM-POSNR.
***                    ELSE.
***                      VAR_NEXT_POSNR = LINES( LT_ZSDT0053_ITEM ).
***                      READ TABLE LT_ZSDT0053_ITEM INTO LW_ZSDT0053_ITEM INDEX VAR_NEXT_POSNR.
***                      VAR_NEXT_POSNR = LW_ZSDT0053_ITEM-POSNR + 10.
***                      LW_ZSDT0053_ITEM-ZMENG = 0.
***                    ENDIF.
***                  ENDIF.
***
***                  LW_ZSDT0053-POSNR           = VAR_NEXT_POSNR.
***                  LW_ZSDT0053-DOC_PRECEDENTE  = LW_ZSDT0053-VBELN.
***                  LW_ZSDT0053-VBELN           = VBAP-VBELN.
***
***                  IF ( LW_ZSDT0053-FIXACAO IS INITIAL ).
***                    LW_ZSDT0053-ZMENG           = VBAP-BRGEW.
***                  ELSE.
***                    LW_ZSDT0053-ZMENG           = LW_ZSDT0053_ITEM-ZMENG + VBAP-BRGEW.
***                  ENDIF.
***
***                  IF ( LW_ZSDT0053-ZIEME EQ 'KG' ) AND ( LW_ZSDT0053-PMEIN EQ 'TO' ).
***                    LW_ZSDT0053-VLRTOT = ( ( ( LW_ZSDT0053-ZMENG * LW_ZSDT0053-DMBTR ) / 1000 ) ).
***                  ELSEIF ( LW_ZSDT0053-ZIEME EQ 'TO' ) AND ( LW_ZSDT0053-PMEIN EQ 'KG' ).
***                    LW_ZSDT0053-VLRTOT = ( ( ( LW_ZSDT0053-ZMENG * LW_ZSDT0053-DMBTR ) * 100 ) ).
***                  ENDIF.
***
***                  LW_ZSDT0053-FIXACAO         = LW_ZSDT0053-FIXACAO.
***
****                   Venda Simples
***                  IF ( LW_ZSDT0053-FIXACAO IS INITIAL ).
***                    SELECT * FROM ZSDT0055
***                      INTO TABLE LT_ZSDT0055
***                    WHERE NRO_SOL_OV = LW_ZSDT0053-NRO_SOL_OV
***                      AND STATUS NE 'C'.
***                    VAR_TABIX = SY-DBCNT.
***                  ELSE.
***
****                    Venda Frame
***                    SELECT * FROM ZSDT0055
***                       INTO TABLE LT_ZSDT0055
***                     WHERE NRO_SOL_OV = LW_ZSDT0053-NRO_SOL_OV
***                       AND FIXACAO    = LW_ZSDT0053-FIXACAO
***                       AND STATUS NE 'C'.
***                    VAR_TABIX = SY-DBCNT.
***
***                    SELECT * FROM ZSDT0073
***                      INTO TABLE LT_ZSDT0073
***                     WHERE NRO_SOL_OV = LW_ZSDT0053-NRO_SOL_OV
***                       AND FIXACAO    = LW_ZSDT0053-FIXACAO.
***
***                    IF ( SY-SUBRC EQ 0 ).
***                      READ TABLE LT_ZSDT0053_AUX TRANSPORTING NO FIELDS WITH KEY STATUS = VAR_DIR
***                                                                                 CHARG  = LW_ZSDT0053-CHARG.
***                      IF ( SY-SUBRC NE 0 ).
***                        READ TABLE LT_ZSDT0073 INTO LW_ZSDT0073 INDEX 1.
***                        LW_ZSDT0073-QTE_VENC = LW_ZSDT0073-QTE_VENC + 1.
***                        UPDATE ZSDT0073 SET QTE_VENC = LW_ZSDT0073-QTE_VENC
***                                      WHERE NRO_SOL_OV = LW_ZSDT0073-NRO_SOL_OV
***                                        AND FIXACAO    = LW_ZSDT0073-FIXACAO.
***                      ENDIF.
***                    ENDIF.
***                  ENDIF.
***
***                  READ TABLE LT_ZSDT0055 INTO LW_ZSDT0055 INDEX VAR_TABIX.
***                  IF ( SY-SUBRC EQ 0 ).
***
***                    IF ( LW_ZSDT0055-DATA_PROGR < SY-DATUM ).
***                      LW_ZSDT0055-DATA_PROGR = SY-DATUM + 30.
***                    ELSE.
***                      LW_ZSDT0055-DATA_PROGR = LW_ZSDT0055-DATA_PROGR.
***                    ENDIF.
***
***                    LW_ZSDT0055-STATUS       = 'D'.
***                    LW_ZSDT0055-CADENCIA_QTE = VBAP-BRGEW.
***                    LW_ZSDT0055-VBELN        = VBAP-VBELN.
***                    LW_ZSDT0055-FIXACAO      = LW_ZSDT0053-FIXACAO.
***
***                    CALL FUNCTION 'NUMBER_GET_NEXT'
***                      EXPORTING
***                        NR_RANGE_NR = '01'
***                        OBJECT      = 'ZSEQ_LOG'
***                      IMPORTING
***                        NUMBER      = LW_ZSDT0055-ID.
***
***                    INSERT INTO ZSDT0055 VALUES LW_ZSDT0055.
***
***                  ELSE.
***                    IF NOT ( LW_ZSDT0053-VALDT IS INITIAL ) AND ( LW_ZSDT0053-VALDT < SY-DATUM ).
***                      LW_ZSDT0053-VALDT = SY-DATUM + 30.
***                    ELSE.
***                      LW_ZSDT0053-VALDT = LW_ZSDT0053-VALDT.
***                    ENDIF.
***                  ENDIF.
***
***                  LW_ZSDT0053-STATUS          = 'D'.
***                  LW_ZSDT0053-JOB             = VAR_DIR.
***                  LW_ZSDT0053-DATA_ATUAL      = SY-DATUM.
***                  LW_ZSDT0053-HORA_ATUAL      = SY-UZEIT.
***
***                  IF ( LW_ZSDT0053-FIXACAO IS INITIAL ).
***                    INSERT INTO ZSDT0053 VALUES LW_ZSDT0053.
***                  ELSE.
***                    READ TABLE LT_ZSDT0053_AUX TRANSPORTING NO FIELDS WITH KEY STATUS = VAR_DIR
***                                                                               CHARG  = LW_ZSDT0053-CHARG.
***                    IF ( SY-SUBRC EQ 0 ).
***                      UPDATE ZSDT0053 SET ZMENG          = LW_ZSDT0053-ZMENG
***                                          VLRTOT         = LW_ZSDT0053-VLRTOT
***                                          VALDT          = LW_ZSDT0053-VALDT
***                                          VBELN          = LW_ZSDT0053-VBELN
***                                          STATUS         = LW_ZSDT0053-STATUS
***                                          JOB            = LW_ZSDT0053-JOB
***                                          USNAM          = SY-UNAME
***                                          DATA_ATUAL     = SY-DATUM
***                                          HORA_ATUAL     = SY-UZEIT
***                                          DOC_PRECEDENTE = LW_ZSDT0053-DOC_PRECEDENTE
***                                          WHERE NRO_SOL_OV = LW_ZSDT0053-NRO_SOL_OV
***                                            AND POSNR      = LW_ZSDT0053-POSNR
***                                            AND FIXACAO    = LW_ZSDT0053-FIXACAO.
***                    ELSE.
***                      INSERT INTO ZSDT0053 VALUES LW_ZSDT0053.
***                    ENDIF.
***                  ENDIF.
***
***                  IF ( SY-SUBRC EQ 0 ).
***
***                    FREE: OBJ_ZCL_WEBSERVICE_TAXA_CURVA.
***                    CREATE OBJECT OBJ_ZCL_WEBSERVICE_TAXA_CURVA.
***
***                    TRY.
****                        Dispara o Hedge Vanda Simples
***                        IF ( LW_ZSDT0053-FIXACAO IS INITIAL ).
***                          IF VAR_DIR EQ 'Y'.
***                              OBJ_ZCL_WEBSERVICE_TAXA_CURVA->EXECUTAR( I_NUMERO  =  LW_ZSDT0053-NRO_SOL_OV
***                                                                       I_TIPO    = 'ENC'
***                                                                       "I_FIXACAO = LW_ZSDT0053-FIXACAO
***                                                                       I_TCODE   = 'VF01'
***                                                                       I_STATUS  =  'Y'
***                                                                       I_VBELN   = VBAP-VBELN ).
***
***                            ELSEIF VAR_DIR EQ 'W'.
***                              OBJ_ZCL_WEBSERVICE_TAXA_CURVA->EXECUTAR( I_NUMERO  =  LW_ZSDT0053-NRO_SOL_OV
***                                                                       I_TIPO    = 'ENC'
***                                                                       "I_FIXACAO = LW_ZSDT0053-FIXACAO
***                                                                       I_TCODE   = 'VF01'
***                                                                       I_AUART   = 'ZCPV'
***                                                                       I_VBELN   = VBAP-VBELN ).
***
***                            ENDIF.
***                        ELSE.
***
****                         Dispara o Hedge Venda Frame
***                          SELECT * FROM ZSDT0056 INTO TABLE LT_ZSDT0056.
***
***                          IF ( SY-SUBRC EQ 0 ).
***
***                            CLEAR: R_BEZEI_LINE.
***                            R_BEZEI_LINE-SIGN   = 'I'.
***                            R_BEZEI_LINE-OPTION = 'EQ'.
***                            R_BEZEI_LINE-LOW    = 'TAXA CAMBIO FRAME'.
***                            R_BEZEI_LINE-HIGH   = 'TAXA CAMBIO FRAME'.
***                            APPEND R_BEZEI_LINE TO R_BEZEI.
***
***                            CLEAR: C_BEZEI_LINE.
***                            C_BEZEI_LINE-SIGN   = 'I'.
***                            C_BEZEI_LINE-OPTION = 'EQ'.
***                            C_BEZEI_LINE-LOW    = 'CHICAGO FRAME'.
***                            C_BEZEI_LINE-HIGH   = 'CHICAGO FRAME'.
***                            APPEND C_BEZEI_LINE TO C_BEZEI.
***
***                            CLEAR: P_BEZEI_LINE.
***                            P_BEZEI_LINE-SIGN   = 'I'.
***                            P_BEZEI_LINE-OPTION = 'EQ'.
***                            P_BEZEI_LINE-LOW    = 'PREMIO FRAME'.
***                            P_BEZEI_LINE-HIGH   = 'PREMIO FRAME'.
***                            APPEND P_BEZEI_LINE TO P_BEZEI.
***
***                            LOOP AT LT_ZSDT0056 INTO LW_ZSDT0056.
***                              VAR_LEN  = STRLEN( LW_ZSDT0056-BEZEI ).
***                              CASE VAR_LEN.
***                                WHEN: '2' OR '3'.
***
***                                  IF ( LW_ZSDT0056-BEZEI(1) EQ 'T' ).
***                                    CLEAR: R_BEZEI_LINE.
***                                    R_BEZEI_LINE-SIGN   = 'I'.
***                                    R_BEZEI_LINE-OPTION = 'EQ'.
***                                    R_BEZEI_LINE-LOW    = LW_ZSDT0056-BEZEI.
***                                    R_BEZEI_LINE-HIGH   = LW_ZSDT0056-BEZEI.
***                                    APPEND R_BEZEI_LINE TO R_BEZEI.
***                                  ENDIF.
***
***                                  IF ( LW_ZSDT0056-BEZEI(1) EQ 'C' ).
***                                    CLEAR: C_BEZEI_LINE.
***                                    C_BEZEI_LINE-SIGN   = 'I'.
***                                    C_BEZEI_LINE-OPTION = 'EQ'.
***                                    C_BEZEI_LINE-LOW    = LW_ZSDT0056-BEZEI.
***                                    C_BEZEI_LINE-HIGH   = LW_ZSDT0056-BEZEI.
***                                    APPEND C_BEZEI_LINE TO C_BEZEI.
***                                  ENDIF.
***
***                                  IF ( LW_ZSDT0056-BEZEI(1) EQ 'P' ).
***                                    CLEAR: P_BEZEI_LINE.
***                                    P_BEZEI_LINE-SIGN   = 'I'.
***                                    P_BEZEI_LINE-OPTION = 'EQ'.
***                                    P_BEZEI_LINE-LOW    = LW_ZSDT0056-BEZEI.
***                                    P_BEZEI_LINE-HIGH   = LW_ZSDT0056-BEZEI.
***                                    APPEND P_BEZEI_LINE TO P_BEZEI.
***                                  ENDIF.
***
***                                WHEN OTHERS.
***                                  CLEAR: LW_ZSDT0056, VAR_LEN.
***                                  CONTINUE.
***                              ENDCASE.
***                              CLEAR: LW_ZSDT0056, VAR_LEN.
***                            ENDLOOP.
***                          ENDIF.
***
****####  INICIO TAXA CAMBIO FRAME
****####  INICIO QTD FIXADA
***                          SELECT * FROM ZSDT0059
***                             INTO TABLE LT_ZSDT0059
***                           WHERE NRO_SOL_OV  EQ LW_ZSDT0053-NRO_SOL_OV
***                             AND POSNR       EQ LW_ZSDT0053-FIXACAO
***                             AND BEZEI       IN R_BEZEI.
***
****                          READ TABLE LT_ZSDT0053_AUX TRANSPORTING NO FIELDS WITH KEY STATUS = VAR_DIR.
****                          IF ( SY-SUBRC EQ 0 ).
***                          LOOP AT LT_ZSDT0059 INTO LW_ZSDT0059 WHERE POSNR1 EQ LW_ZSDT0053-POSNR.
***                            VAR_BEZEI = LW_ZSDT0059-BEZEI.
***                          ENDLOOP.
****                          ELSE.
***                          LOOP AT LT_ZSDT0059 INTO LW_ZSDT0059.
***                            IF NOT ( LW_ZSDT0059-VALDT_HEDGE IS INITIAL ).
***                              CONTINUE.
***                            ELSEIF ( VAR_BEZEI IS INITIAL ).
***                              VAR_BEZEI = LW_ZSDT0059-BEZEI.
***                            ENDIF.
***                            CLEAR: LW_ZSDT0059.
***                          ENDLOOP.
****                          ENDIF.
***
***                          VAR_FORMULA = LW_ZSDT0053-ZMENG.
***                          CONDENSE VAR_FORMULA NO-GAPS.
***
***                          UPDATE ZSDT0059 SET FORMULA      = VAR_FORMULA
***                                              FORMULA2     = VAR_FORMULA
***                                              POSNR1       = LW_ZSDT0053-POSNR
***                                              MONAT        = LW_ZSDT0059-MONAT
***                                              VALDT        = SY-DATUM
***                                              USNAM        = SY-UNAME
***                                              DATA_ATUAL   = SY-DATUM
***                                              HORA_ATUAL   = SY-UZEIT
***                                              VALDT_HEDGE  = '00000000'
***                                          WHERE NRO_SOL_OV = LW_ZSDT0053-NRO_SOL_OV
***                                            AND POSNR      = LW_ZSDT0053-FIXACAO
***                                            AND BEZEI      = VAR_BEZEI
***                                            AND FIELD      = 'QTDFIXADA'.
***
****                                REFRESH: LT_ZSDT0059[].
****####  FIM QTD FIXADA
***
****####  INICIO PRECO
***
***                          READ TABLE LT_ZSDT0059 INTO LW_ZSDT0059 WITH KEY NRO_SOL_OV  = LW_ZSDT0053-NRO_SOL_OV
***                                                                           POSNR       = LW_ZSDT0053-FIXACAO
***                                                                           BEZEI       = 'TAXA CAMBIO FRAME'
***                                                                           FIELD       = 'PRECO'.
***                          CLEAR VAR_FORMULA.
***                          VAR_FORMULA = LW_ZSDT0059-FORMULA2.
***                          CONDENSE VAR_FORMULA NO-GAPS.
***                          CLEAR LW_ZSDT0059.
***
***                          READ TABLE LT_ZSDT0059 INTO LW_ZSDT0059 WITH KEY NRO_SOL_OV  = LW_ZSDT0053-NRO_SOL_OV
***                                                                           POSNR       = LW_ZSDT0053-FIXACAO
***                                                                           BEZEI       = 'T1'
***                                                                           FIELD       = 'PRECO'.
***
***                          IF ( SY-SUBRC EQ 0 ).
***                            UPDATE ZSDT0059 SET FORMULA      = VAR_FORMULA
***                                                FORMULA2     = VAR_FORMULA
***                                                POSNR1       = LW_ZSDT0053-POSNR
***                                                MONAT        = LW_ZSDT0059-MONAT
***                                                VALDT        = SY-DATUM
***                                                USNAM        = SY-UNAME
***                                                DATA_ATUAL   = SY-DATUM
***                                                HORA_ATUAL   = SY-UZEIT
***                                                VALDT_HEDGE  = '00000000'
***                                            WHERE NRO_SOL_OV = LW_ZSDT0053-NRO_SOL_OV
***                                              AND POSNR      = LW_ZSDT0053-FIXACAO
***                                              AND BEZEI      = VAR_BEZEI
***                                              AND FIELD      = 'PRECO'.
***                          ENDIF.
***
***                          REFRESH: LT_ZSDT0059[].
***                          CLEAR: LW_ZSDT0059.
****####  FIM PRECO
****####  FIM TAXA CAMBIO FRAME
***
****####  INICIO PREMIO FRAME
****####  INICIO QTD FIXADA
***                          SELECT * FROM ZSDT0059
***                             INTO TABLE LT_ZSDT0059
***                           WHERE NRO_SOL_OV  EQ LW_ZSDT0053-NRO_SOL_OV
***                             AND POSNR       EQ LW_ZSDT0053-FIXACAO
***                             AND BEZEI       IN P_BEZEI.
***
***                          CLEAR: VAR_BEZEI.
***                          LOOP AT LT_ZSDT0059 INTO LW_ZSDT0059 WHERE POSNR1 EQ LW_ZSDT0053-POSNR.
***                            VAR_BEZEI = LW_ZSDT0059-BEZEI.
***                          ENDLOOP.
***
***                          LOOP AT LT_ZSDT0059 INTO LW_ZSDT0059.
***                            IF NOT ( LW_ZSDT0059-VALDT IS INITIAL ).
***                              CONTINUE.
***                            ELSEIF ( VAR_BEZEI IS INITIAL ).
***                              VAR_BEZEI = LW_ZSDT0059-BEZEI.
***                            ENDIF.
***                            CLEAR: LW_ZSDT0059.
***                          ENDLOOP.
***
***                          READ TABLE LT_ZSDT0059 INTO LW_ZSDT0059 WITH KEY BEZEI = 'P1'.
***                          VAR_FORMULA = LW_ZSDT0053-ZMENG.
***                          CONDENSE VAR_FORMULA NO-GAPS.
***
***                          UPDATE ZSDT0059 SET FORMULA      = VAR_FORMULA
***                                              FORMULA2     = VAR_FORMULA
***                                              POSNR1       = LW_ZSDT0053-POSNR
***                                              CBOT         = LW_ZSDT0059-CBOT
***                                              MONAT        = LW_ZSDT0059-MONAT
***                                              VALDT        = SY-DATUM
***                                              USNAM        = SY-UNAME
***                                              DATA_ATUAL   = SY-DATUM
***                                              HORA_ATUAL   = SY-UZEIT
***                                              VALDT_HEDGE  = '00000000'
***                                          WHERE NRO_SOL_OV = LW_ZSDT0053-NRO_SOL_OV
***                                            AND POSNR      = LW_ZSDT0053-FIXACAO
***                                            AND BEZEI      = VAR_BEZEI
***                                            AND FIELD      = 'QTDFIXADA'.
***
****####  FIM QTD FIXADA
***
****####  INICIO PREÇO
***                          READ TABLE LT_ZSDT0059 INTO LW_ZSDT0059 WITH KEY NRO_SOL_OV  = LW_ZSDT0053-NRO_SOL_OV
***                                                                           POSNR       = LW_ZSDT0053-FIXACAO
***                                                                           BEZEI       = 'PREMIO FRAME'
***                                                                           FIELD       = 'PRECO'.
***                          CLEAR VAR_FORMULA.
***                          VAR_FORMULA = LW_ZSDT0059-FORMULA2.
***                          CONDENSE VAR_FORMULA NO-GAPS.
***                          CLEAR LW_ZSDT0059.
***
***                          READ TABLE LT_ZSDT0059 INTO LW_ZSDT0059 WITH KEY NRO_SOL_OV  = LW_ZSDT0053-NRO_SOL_OV
***                                                                           POSNR       = LW_ZSDT0053-FIXACAO
***                                                                           BEZEI       = 'P1'
***                                                                           FIELD       = 'PRECO'.
***
***                          IF ( SY-SUBRC EQ 0 ).
***
***                            UPDATE ZSDT0059 SET FORMULA      = VAR_FORMULA
***                                                FORMULA2     = VAR_FORMULA
***                                                POSNR1       = LW_ZSDT0053-POSNR
***                                                CBOT         = LW_ZSDT0059-CBOT
***                                                MONAT        = LW_ZSDT0059-MONAT
***                                                VALDT        = SY-DATUM
***                                                USNAM        = SY-UNAME
***                                                DATA_ATUAL   = SY-DATUM
***                                                HORA_ATUAL   = SY-UZEIT
***                                                VALDT_HEDGE  = '00000000'
***                                            WHERE NRO_SOL_OV = LW_ZSDT0053-NRO_SOL_OV
***                                              AND POSNR      = LW_ZSDT0053-FIXACAO
***                                              AND BEZEI      = VAR_BEZEI
***                                              AND FIELD      = 'PRECO'.
***                          ENDIF.
***
***                          REFRESH: LT_ZSDT0059[].
***                          CLEAR: LW_ZSDT0059.
****####  FIM PRECO
****####  FIM PREMIO FRAME
***
****####  INICIO CHICAGO FRAME
****####  INICIO QTD FIXADA
***                          SELECT * FROM ZSDT0059
***                             INTO TABLE LT_ZSDT0059
***                           WHERE NRO_SOL_OV  EQ LW_ZSDT0053-NRO_SOL_OV
***                             AND POSNR       EQ LW_ZSDT0053-FIXACAO
***                             AND BEZEI       IN C_BEZEI.
***
***                          CLEAR: VAR_BEZEI.
***                          LOOP AT LT_ZSDT0059 INTO LW_ZSDT0059 WHERE POSNR1 EQ LW_ZSDT0053-POSNR.
***                            VAR_BEZEI = LW_ZSDT0059-BEZEI.
***                          ENDLOOP.
***
***                          LOOP AT LT_ZSDT0059 INTO LW_ZSDT0059.
***                            IF NOT ( LW_ZSDT0059-VALDT IS INITIAL ).
***                              CONTINUE.
***                            ELSEIF ( VAR_BEZEI IS INITIAL ).
***                              VAR_BEZEI = LW_ZSDT0059-BEZEI.
***                            ENDIF.
***                            CLEAR: LW_ZSDT0059.
***                          ENDLOOP.
***
***                          READ TABLE LT_ZSDT0059 INTO LW_ZSDT0059 WITH KEY BEZEI = 'C1'.
***                          VAR_FORMULA = LW_ZSDT0053-ZMENG.
***                          CONDENSE VAR_FORMULA NO-GAPS.
***
***                          UPDATE ZSDT0059 SET FORMULA      = VAR_FORMULA
***                                              FORMULA2     = VAR_FORMULA
***                                              POSNR1       = LW_ZSDT0053-POSNR
***                                              CBOT         = LW_ZSDT0059-CBOT
***                                              MONAT        = LW_ZSDT0059-MONAT
***                                              VALDT        = SY-DATUM
***                                              USNAM        = SY-UNAME
***                                              DATA_ATUAL   = SY-DATUM
***                                              HORA_ATUAL   = SY-UZEIT
***                                              VALDT_HEDGE  = '00000000'
***                                          WHERE NRO_SOL_OV = LW_ZSDT0053-NRO_SOL_OV
***                                            AND POSNR      = LW_ZSDT0053-FIXACAO
***                                            AND BEZEI      = VAR_BEZEI
***                                            AND FIELD      = 'QTDFIXADA'.
***
****####  FIM QTD FIXADA
***
****####  INICIO PRECO
***                          READ TABLE LT_ZSDT0059 INTO LW_ZSDT0059 WITH KEY NRO_SOL_OV  = LW_ZSDT0053-NRO_SOL_OV
***                                                                           POSNR       = LW_ZSDT0053-FIXACAO
***                                                                           BEZEI       = 'CHICAGO FRAME'
***                                                                           FIELD       = 'PRECO'.
***                          CLEAR VAR_FORMULA.
***                          VAR_FORMULA = LW_ZSDT0059-FORMULA2.
***                          CONDENSE VAR_FORMULA NO-GAPS.
***                          CLEAR LW_ZSDT0059.
***
***                          READ TABLE LT_ZSDT0059 INTO LW_ZSDT0059 WITH KEY NRO_SOL_OV  = LW_ZSDT0053-NRO_SOL_OV
***                                                                           POSNR       = LW_ZSDT0053-FIXACAO
***                                                                           BEZEI       = 'C1'
***                                                                           FIELD       = 'PRECO'.
***
***
***                          IF ( SY-SUBRC EQ 0 ).
***
***                            UPDATE ZSDT0059 SET FORMULA      = VAR_FORMULA
***                                                FORMULA2     = VAR_FORMULA
***                                                POSNR1       = LW_ZSDT0053-POSNR
***                                                CBOT         = LW_ZSDT0059-CBOT
***                                                MONAT        = LW_ZSDT0059-MONAT
***                                                VALDT        = SY-DATUM
***                                                USNAM        = SY-UNAME
***                                                DATA_ATUAL   = SY-DATUM
***                                                HORA_ATUAL   = SY-UZEIT
***                                                VALDT_HEDGE  = '00000000'
***                                            WHERE NRO_SOL_OV = LW_ZSDT0053-NRO_SOL_OV
***                                              AND POSNR      = LW_ZSDT0053-FIXACAO
***                                              AND BEZEI      = VAR_BEZEI
***                                              AND FIELD      = 'PRECO'.
***                          ENDIF.
***
***                          REFRESH: LT_ZSDT0059[].
***                          CLEAR: LW_ZSDT0059.
****####  FIM PRECO
****####  FIM CHICAGO FRAME
***
****                         Insere na 0100 todas as alterações nos itens.
****######################## inicio
***                          CALL FUNCTION 'NUMBER_GET_NEXT'
***                            EXPORTING
***                              NR_RANGE_NR = '01'
***                              OBJECT      = 'ZSEQ_HEDGE'
***                            IMPORTING
***                              NUMBER      = LW_ZSDT0100-SEQ.
***
***                          LW_ZSDT0100-NRO_SOL_OV  = LW_ZSDT0053-NRO_SOL_OV.
***                          LW_ZSDT0100-FIXACAO     = LW_ZSDT0053-FIXACAO.
***                          LW_ZSDT0100-POSNR       = LW_ZSDT0053-POSNR.
***                          LW_ZSDT0100-VBELN       = VBAK-VBELN.
***                          LW_ZSDT0100-AUART       = VBAK-AUART.
***                          LW_ZSDT0100-ZMENG       = VBAP-BRGEW.
***                          LW_ZSDT0100-ZIEME       = VBAP-ZIEME.
***                          LW_ZSDT0100-VBELV       = VBAK-VGBEL.
***                          LW_ZSDT0100-STATUS      = VAR_DIR.
***                          LW_ZSDT0100-USNAM       = SY-UNAME.
***                          LW_ZSDT0100-DATA_ATUAL  = SY-DATUM.
***                          LW_ZSDT0100-HORA_ATUAL  = SY-UZEIT.
***
***                          INSERT INTO ZSDT0100 VALUES LW_ZSDT0100.
****######################## Fim
***                            OBJ_ZCL_WEBSERVICE_TAXA_CURVA->EXECUTAR(  I_NUMERO  =  LW_ZSDT0053-NRO_SOL_OV
***                                                              I_TIPO    = 'FRA'
***                                                              I_FIXACAO = LW_ZSDT0053-FIXACAO
***                                                              I_TCODE   = 'VF01'
***                                                              I_VBELN   = LW_ZSDT0053-VBELN
***                                                              I_AUART   = TKOMK-AUART_SD
***                                                              ).
***                         IF _MOEDA EQ 'BRL'.
***                            OBJ_ZCL_WEBSERVICE_TAXA_CURVA->EXECUTAR(  I_NUMERO  = LW_ZSDT0053-NRO_SOL_OV
***                                                                      I_TIPO    = 'FRE'
***                                                                      I_FIXACAO = LW_ZSDT0053-FIXACAO
****                                                                          I_TCODE   = 'ZSDT0062'
***                                                                      I_TCODE   = 'VF01'
***                                                                      I_STATUS  = 'X'
***                                                                      I_VBELN   = LW_ZSDT0053-VBELN
***                                                                      I_AUART   = TKOMK-AUART_SD
***                                                                      ).
***                          ENDIF.
***                          ENDIF.
***
***                      CATCH ZCX_WEBSERVICE INTO CX_EXCEPTION.
***                        VAR_MSG = CX_EXCEPTION->GET_TEXT( ).
***                        MESSAGE E007(ZWEBSERVICE) DISPLAY LIKE 'W' WITH VAR_MSG.
***
***                    ENDTRY.
***                  ENDIF.
***                ENDIF.
***              ENDIF.
***            ENDIF.
***          ENDIF.
***        ENDIF.
***
***        CALL FUNCTION 'ENQUEUE_EZSDT0051'
***          EXPORTING
***            NRO_SOL_OV = LW_ZSDT0053_EQU-NRO_SOL_OV.
***      ELSE.
***
***        DATA MAT_CHARG TYPE C LENGTH 50.
***
***        CHECK DIR_MI_IN EQ 'IN'.
***
***        IF ( SY-UCOMM EQ 'SICH' ).
***
***          CASE TKOMK-AUART_SD.
***            WHEN: 'ZROB' OR 'ZREB'
***               OR 'ZRPF' OR 'ZRPF'.
***
***              DATA: VL_TRAVA_CAMBIO TYPE C,
***                    WL_0040_AUX     TYPE ZSDT0040.
***
***              CLEAR WA_INSERT.
***              LOOP AT IT_INSERT INTO WA_INSERT.
***
***                SELECT SINGLE *
***                  FROM ZSDT0040 INTO WL_0040_AUX
***                 WHERE DOC_SIMULACAO EQ WA_INSERT-DOC.
***
***                CHECK SY-SUBRC = 0.
***
***                SELECT COUNT(*) FROM ZSDT0090
***                  WHERE VBELN EQ WA_INSERT-VBELN
***                  AND  MATNR EQ WA_INSERT-MATNR
***                  AND CHARG EQ WA_INSERT-CHARG
***                  AND CATEGORIA EQ VAR_DIR
***                  AND ESTORNO EQ ABAP_FALSE.
***
***                IF NOT SY-SUBRC IS INITIAL.
***
***                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
***                    EXPORTING
***                      INPUT  = WA_INSERT-CHARG
***                    IMPORTING
***                      OUTPUT = WA_INSERT-CHARG.
***
***                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
***                    EXPORTING
***                      INPUT  = WA_INSERT-MATNR
***                    IMPORTING
***                      OUTPUT = WA_INSERT-MATNR.
***
****                                   10 CARACTERES       18 CARACTERES
***                  MAT_CHARG = |{ WA_INSERT-CHARG }#{ WA_INSERT-MATNR }|.
***
***                  VCATEGORIA = VAR_DIR.
***
***                   CASE VAR_DIR.
***                     WHEN 'Y' OR 'W'.
***                            OBJ->INSERT_ZSDT90(
***                              EXPORTING
***                                DIRECAO   =  VCATEGORIA
***                                SIMULADOR =  WA_INSERT-DOC    " Numero do documento de simulação de venda
***                                ORDEM_OLD =  WA_INSERT-VBELV   " Nº documento de vendas e distribuição
***                                ORDEM_NEW =  WA_INSERT-VBELN   " Nº documento de vendas e distribuição
***                                MATERIAL  =  WA_INSERT-MATNR  " Nº do material
***                                CHARG     =  WA_INSERT-CHARG   " Número do lote
***                              RECEIVING
***                                RETURN    =  P_0090
***                            ).
***
***                    WHEN OTHERS.
***
***                      PERFORM INSERT_ZSDT0090(ZSDR0042)
***                       USING VAR_DIR
***                             WA_INSERT-DOC      " DOC_SIMULACAO
***                             WA_INSERT-VBELN    " NEW
***                             WA_INSERT-VBELV    " OLD
***                             MAT_CHARG    " LOTE E MATERIAL
***                       CHANGING P_0090.
***                   ENDCASE.
***
***
***                  MOVE-CORRESPONDING P_0090 TO IM_0090.
***
***                  CLEAR: VL_TRAVA_CAMBIO.
***                  IF WL_0040_AUX-WAERK EQ 'USD'.
***                    CALL FUNCTION 'ZSDMF001_CHECK_OV_TRAVA_CAMBIO'
***                      EXPORTING
***                        I_DOC_SIMULACAO = WA_INSERT-DOC
***                        I_VBELN         = WA_INSERT-VBELV
***                      CHANGING
***                        C_TRAVA_CAMBIO  = VL_TRAVA_CAMBIO.
***                  ENDIF.
***
***                  IF ( WL_0040_AUX-WAERK EQ 'BRL' ) OR
***
***                     ( ( WL_0040_AUX-WAERK EQ 'USD' ) AND
***                       ( VL_TRAVA_CAMBIO IS NOT INITIAL ) ).
***
***                    ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS( I_0090 = IM_0090
***                                                            I_TIPO = 'VDI'
***                                                            I_DIR  = VAR_DIR
***                                                           ).
***                  ENDIF.
***                  ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS( I_0090 = IM_0090
***                                                          I_TIPO = 'FRI'
***                                                          I_DIR  = VAR_DIR
***                                                         ).
***                  CLEAR: P_0090, IM_0090.
***                ENDIF.
***              ENDLOOP.
***          ENDCASE.
***        ENDIF.
***      ENDIF.
***    ENDIF.

    IF sy-ucomm = 'SICH'.
      IF wa_mara-mtart = 'ZFER' AND tkomk-auart_sd IN r_auart_dev.  "<<RIM-SKM-CS1025614-15.09.22

        IMPORT vbap-vbeln TO var_transf FROM MEMORY ID 'VDOC'.
        "             EXPORT: VBAP-VBELN TO MEMORY ID 'VDOC'.

        IF vbrp-werks = '0175'.

          IF var_transf <> vbap-vbeln.

            CLEAR msg.
            "bloqueio
            fg_bloqueio      = 'X'.
            CALL FUNCTION 'ENQUEUE_EMMARCE'
              EXPORTING
                matnr          = vbrp-matnr
                werks          = vbrp-werks
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.

            sperr_user =  sy-msgv1.

            IF sy-subrc = 1.
              CONCATENATE 'Material ' vbrp-matnr 'bloqueado por ' sperr_user INTO msg SEPARATED BY space.
              MESSAGE msg TYPE 'E'.
              EXIT.
            ENDIF.

            IF vbrp-charg IS NOT INITIAL.

              fg_bloqueio      = 'X'.
              CALL FUNCTION 'ENQUEUE_EMMCH1E'
                EXPORTING
                  mode_mch1      = 'E'
                  mandt          = sy-mandt
                  matnr          = vbrp-matnr
                  charg          = vbrp-charg
                  _scope         = '2'
                EXCEPTIONS
                  foreign_lock   = 1
                  system_failure = 2
                  OTHERS         = 3.

              sperr_user  = sy-msgv1.

              IF sy-subrc = 1.
                CONCATENATE 'Lote ' vbrp-charg 'bloqueado por ' sperr_user INTO msg SEPARATED BY space.
                MESSAGE msg TYPE 'E'.
                EXIT.
              ENDIF.

            ENDIF.


            REFRESH : tl_item, tl_return.
            "iniciando a bapi
            vl_code = '04'.
            sl_header-pstng_date      =  sy-datum.
            sl_header-doc_date        =  sy-datum.
            sl_header-ref_doc_no      = vbap-vbeln.
            sl_header-ref_doc_no_long = vbap-vbeln.

            sl_item-move_type          = '411'.
            sl_item-spec_stock         = 'E'.
* ---> S4 Migration - 04/07/2023 - FTM - Início
            sl_item-material           = vbrp-matnr.
            DATA(v_len) = strlen( vbrp-matnr ).
            IF v_len > 18.
              sl_item-material_long = vbrp-matnr.
            ELSE.
              sl_item-material      = vbrp-matnr.
            ENDIF.
* <--- S4 Migration - 04/07/2023 - FTM - Fim
            sl_item-plant              = vbrp-werks.
            sl_item-stge_loc           = vbrp-lgort.
            sl_item-batch              = vbrp-charg.
            sl_item-val_sales_ord      = vbap-vbeln.
            sl_item-val_s_ord_item     = vbap-posnr.
            sl_item-entry_qnt          = vbrp-fkimg.
            sl_item-entry_uom          = vbrp-meins.
            sl_item-sales_ord          = vbrp-vgbel.
            sl_item-s_ord_item         = vbrp-vgpos.

            sl_item-move_mat           = wa_zppt0017-matnr_rv.
            sl_item-move_plant         = '0175'.
            sl_item-move_stloc         = 'IN01'.
            sl_item-move_batch         = vbrp-charg.

            es_bflushflags-bckfltype   = '01'.

            es_bflushdatagen-postdate    = sy-datum.
            es_bflushdatagen-prodplant   = vbrp-werks.
            es_bflushdatagen-materialnr  = vbrp-matnr.
            es_bflushdatagen-backflquant = vbrp-fkimg.
            es_bflushdatagen-batch       = vbrp-charg.

            APPEND sl_item TO tl_item.

            CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
              EXPORTING
                goodsmvt_header  = sl_header
                goodsmvt_code    = vl_code
              IMPORTING
                materialdocument = vl_doc
                matdocumentyear  = vl_year
              TABLES
                goodsmvt_item    = tl_item
                return           = tl_return.

            READ TABLE tl_return INTO sl_return WITH KEY type = 'E'.
            IF sy-subrc NE 0.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.
              CLEAR sl_return.
            ELSE.
              MESSAGE sl_return-message TYPE 'E'.
              EXIT.
            ENDIF.

            IF fg_bloqueio    = 'X'.
              CALL FUNCTION 'DEQUEUE_EMMARCE' "#EC CI_FLDEXT_OK[2215424]
                EXPORTING
                  matnr = sl_item-material "#EC CI_FLDEXT_OK[2215424]
                  werks = sl_item-plant.

              IF sl_item-batch IS NOT INITIAL.
                CALL FUNCTION 'DEQUEUE_EMMCH1E' "#EC CI_FLDEXT_OK[2215424]
                  EXPORTING
                    mode_mch1 = 'E'
                    mandt     = sy-mandt
                    matnr     = sl_item-material "#EC CI_FLDEXT_OK[2215424]
                    charg     = sl_item-batch.
              ENDIF.
            ENDIF.

            EXPORT: vbap-vbeln TO MEMORY ID 'VDOC'.
            "  VAR_TRANSF = 'X'.

          ENDIF.
        ENDIF. "<<RIM-SKM-CS1025614-15.09.22
      ENDIF.
    ENDIF.


************VF11***VF11*****VF11****VF11*****VF11*****VF11*****VF11*****VF11*************
**********************************ESTORNO************************************************
  WHEN: 'VF11'.

    IF wa_mara-mtart = 'ZFER'.
      IF tkomk-auart_sd IN r_auart_dev.    "<<RIM-SKM-CS1025614-15.09.22
*      IF TKOMK-AUART_SD = 'ZROB' OR TKOMK-AUART_SD = 'ZREB' OR  TKOMK-AUART_SD = 'ZRPF'. ">>RIM-SKM-CS1025614-15.09.22
        IF tkomk-werks = '0175'.
          SELECT SINGLE * FROM zppt0017 INTO wa_zppt0017
            WHERE situacao EQ 'A'
            AND   werks    EQ tkomk-werks
            AND   matnr_ac EQ vbap-matnr.

          IF  wa_zppt0017 IS INITIAL.
            MESSAGE 'Não Existe DE/PARA de material cadastrado na transação ZPP0015!' TYPE 'E'.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.


    IF ( sy-ucomm EQ 'SICH').
***      IF TKOMK-AUART_SD IN R_AUART.
***
***        SELECT COUNT(*) FROM ZSDT0053
***         WHERE VBELN EQ VBAK-VBELN
***           AND STATUS IN ('W' , 'Y' ).
***
***        IF SY-SUBRC IS INITIAL.
***          DIR_MI_IN = 'MI'.
***        ELSE.
***
***          SELECT COUNT(*) FROM ZSDT0100
***            WHERE VBELN EQ VBAK-VBELN
***            AND STATUS NE 'C'.
***
***          IF SY-SUBRC IS INITIAL.
***            DIR_MI_IN = 'MI'.
***          ENDIF.
***        ENDIF.
***
***        SELECT COUNT(*) FROM ZSDT0090
***          WHERE VBELN EQ VBAP-VBELN
***          AND  POSNN EQ VBAP-POSNR
***          AND CATEGORIA EQ VAR_DIR
***          AND ESTORNO EQ ABAP_FALSE.
***
***        IF SY-SUBRC IS INITIAL.
***          DIR_MI_IN = 'IN'.
***        ENDIF.
***
***        CASE DIR_MI_IN.
***          WHEN 'MI'.
***
***            SELECT * FROM ZSDT0053
***               INTO TABLE LT_ZSDT0053
***             WHERE VBELN EQ VBAK-VBELN
***               AND STATUS IN ('W' , 'Y' ).
***
***            SELECT * FROM ZSDT0051
***               INTO TABLE IT_ZSDT0051
***               FOR  ALL ENTRIES IN LT_ZSDT0053
***             WHERE NRO_SOL_OV EQ LT_ZSDT0053-NRO_SOL_OV.
***
***            CHECK IT_ZSDT0051[ 1 ]-WAERK EQ 'BRL'.
***
***            SELECT * FROM ZSDT0100
***              INTO TABLE LT_ZSDT0100
***              WHERE VBELN EQ VBAK-VBELN.
***
***            DELETE LT_ZSDT0100 WHERE STATUS EQ 'C'.
***
***            IF IT_ZSDT0051[] IS INITIAL.
***              SELECT * FROM ZSDT0051
***                 APPENDING TABLE IT_ZSDT0051
***                 FOR  ALL ENTRIES IN LT_ZSDT0100
***               WHERE NRO_SOL_OV EQ LT_ZSDT0100-NRO_SOL_OV.
***            ENDIF.
***
***            TRY.
***                TP_VENDA = IT_ZSDT0051[ NRO_SOL_OV = LT_ZSDT0053[ 1 ]-NRO_SOL_OV ]-TP_VENDA.
***              CATCH CX_SY_ITAB_LINE_NOT_FOUND.
***                TRY.
***                    TP_VENDA = IT_ZSDT0051[ NRO_SOL_OV = LT_ZSDT0100[ 1 ]-NRO_SOL_OV ]-TP_VENDA.
***                  CATCH CX_SY_ITAB_LINE_NOT_FOUND.
***                    CLEAR TP_VENDA.
***                ENDTRY.
***            ENDTRY.
***
***            SELECT COUNT(*)
***              FROM SETLEAF
***              WHERE SETNAME EQ 'MAGGI_ZSDT0062_HEDGE'
***                AND VALFROM EQ TP_VENDA.
***
***            CHECK SY-SUBRC IS INITIAL.
***
***            IF LINES( LT_ZSDT0100 ) EQ 0.
***              READ TABLE LT_ZSDT0053 INTO LW_ZSDT0053 INDEX 1.
***              IF NOT SY-SUBRC IS INITIAL.
***                EXIT.
***              ENDIF.
***            ENDIF.
***
***            READ TABLE LT_ZSDT0053 INTO LW_ZSDT0053 INDEX 1.
***            IF SY-SUBRC EQ 0 AND LW_ZSDT0053-FIXACAO IS INITIAL.
***              UPDATE ZSDT0053 SET STATUS = 'C'
****                                VBELN = ''
***                                WHERE VBELN EQ VBAK-VBELN.
***            ELSE.
***              IF LT_ZSDT0100 IS NOT INITIAL.
***                UPDATE ZSDT0100 SET STATUS = 'C'
***                                    WHERE VBELN EQ VBAK-VBELN.
***              ELSE.
***                UPDATE ZSDT0053 SET STATUS = 'C'
****                                    VBELN = ''
***                                    WHERE VBELN EQ VBAK-VBELN.
***              ENDIF.
***            ENDIF.
***
***            UPDATE ZSDT0055
***             SET STATUS = 'C'
****                  VBELN = ''
***            WHERE VBELN EQ VBAK-VBELN.
***
***            READ TABLE LT_ZSDT0100 INTO LW_ZSDT0100 INDEX 1.
***            FREE: OBJ_ZCL_WEBSERVICE_TAXA_CURVA.
***            CREATE OBJECT OBJ_ZCL_WEBSERVICE_TAXA_CURVA.
***            TRY.
***                IF ( LW_ZSDT0053-FIXACAO IS INITIAL ) AND ( LW_ZSDT0100-FIXACAO IS INITIAL ).
***                  OBJ_ZCL_WEBSERVICE_TAXA_CURVA->EXECUTAR( I_NUMERO =  LW_ZSDT0053-NRO_SOL_OV
***                                                           I_TIPO   = 'EDI'
***                                                           I_TCODE  = 'VF11'
***                                                           I_VBELN  = VBAK-VBELN
***                                                           ).
***                ELSE.
***                  IF LW_ZSDT0053 IS NOT INITIAL.
***                    OBJ_ZCL_WEBSERVICE_TAXA_CURVA->EXECUTAR( I_NUMERO  =  LW_ZSDT0053-NRO_SOL_OV
***                                                             I_TIPO    = 'EDF'
***                                                             I_STATUS  = LW_ZSDT0053-STATUS
***                                                             I_FIXACAO = LW_ZSDT0053-FIXACAO
***                                                             I_TCODE   = 'VF11'
***                                                             I_VBELN   = VBAK-VBELN
***                                                             I_AUART   = TKOMK-AUART_SD
***                                                             ).
***                  ELSE.
***                    OBJ_ZCL_WEBSERVICE_TAXA_CURVA->EXECUTAR( I_NUMERO  =  LW_ZSDT0100-NRO_SOL_OV
***                                                             I_TIPO    = 'EDF'
***                                                             I_STATUS  = LW_ZSDT0100-STATUS
***                                                             I_FIXACAO = LW_ZSDT0100-FIXACAO
***                                                             I_TCODE   = 'VF11'
***                                                             I_VBELN   = VBAK-VBELN
***                                                             I_AUART   = TKOMK-AUART_SD
***                                                             ).
***                  ENDIF.
***                ENDIF.
***
***              CATCH ZCX_WEBSERVICE INTO CX_EXCEPTION.
***                VAR_MSG = CX_EXCEPTION->GET_TEXT( ).
***                MESSAGE E007(ZWEBSERVICE) DISPLAY LIKE 'W' WITH VAR_MSG.
***            ENDTRY.
***
***          WHEN 'IN'.
***
***            UPDATE ZSDT0090  SET ESTORNO = ABAP_TRUE
***                                 USNAM_E       = SY-UNAME
***                                 DATA_ATUAL_E  = SY-DATUM
***                                 HORA_ATUAL_E  = SY-UZEIT
***                                 ORIGEM_EST    = SY-CPROG
***                   WHERE VBELN EQ VBAP-VBELN
***                     AND CATEGORIA EQ VAR_DIR
***                     AND ESTORNO EQ ABAP_FALSE.
***
***            IF SY-SUBRC IS INITIAL.
***              COMMIT WORK.
***
***              ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS( I_TIPO   = 'EST'
***                                                      I_VBELN  = VBAP-VBELN
***                                                      I_DIR    = VAR_DIR
***                                                    ).
***            ENDIF.
***
***        ENDCASE.
***      ENDIF.

*     "// Hedge Aquaviario
      zcl_webservice_tx_curva=>hedge_aquaviario(
                                                 _code = sy-tcode
                                                 _vbrk = vbrk
                                               ).

      IF vbrp-werks = '0175'.
        IF wa_mara-mtart = 'ZFER' AND tkomk-auart_sd IN r_auart_dev.    "<<RIM-SKM-CS1025614-15.09.22

          CLEAR var_transf.

          SELECT * FROM mseg INTO TABLE @DATA(it_mseg)
            WHERE kdauf EQ @vbak-vbeln
            AND   bwart EQ '411'.

          LOOP AT it_mseg INTO DATA(wa_mseg).

            SELECT SINGLE * FROM mseg INTO @DATA(wa_mseg_aux)
              WHERE smbln EQ @wa_mseg-mblnr
              AND   bwart EQ '412'.

            IF sy-subrc = 4.
              IMPORT vbap-vbeln TO var_transf FROM MEMORY ID 'VDOC'.
              IF var_transf <> vbap-vbeln.
                "bloqueio
                fg_bloqueio      = 'X'.
                CALL FUNCTION 'ENQUEUE_EMMARCE'
                  EXPORTING
                    matnr          = wa_zppt0017-matnr_ac
                    werks          = wa_zppt0017-werks
                  EXCEPTIONS
                    foreign_lock   = 1
                    system_failure = 2
                    OTHERS         = 3.

                sperr_user =  sy-msgv1.

                IF sy-subrc = 1.
                  CONCATENATE 'Material ' wa_zppt0017-matnr_ac 'bloqueado por ' sperr_user INTO msg SEPARATED BY space.
                  MESSAGE msg TYPE 'E'.
                  EXIT.
                ENDIF.

                "bloqueio
                fg_bloqueio      = 'X'.
                CALL FUNCTION 'ENQUEUE_EMMARCE'
                  EXPORTING
                    matnr          = wa_zppt0017-matnr_rv
                    werks          = wa_zppt0017-werks
                  EXCEPTIONS
                    foreign_lock   = 1
                    system_failure = 2
                    OTHERS         = 3.

                sperr_user =  sy-msgv1.

                IF sy-subrc = 1.
                  CONCATENATE 'Material ' wa_zppt0017-matnr_rv 'bloqueado por ' sperr_user INTO msg SEPARATED BY space.
                  MESSAGE msg TYPE 'E'.
                  EXIT.
                ENDIF.


                IF vbrp-charg IS NOT INITIAL.
                  fg_bloqueio      = 'X'.
                  CALL FUNCTION 'ENQUEUE_EMMCH1E'
                    EXPORTING
                      mode_mch1      = 'E'
                      mandt          = sy-mandt
                      matnr          = vbrp-matnr
                      charg          = vbrp-charg
                      _scope         = '2'
                    EXCEPTIONS
                      foreign_lock   = 1
                      system_failure = 2
                      OTHERS         = 3.

                  sperr_user  = sy-msgv1.

                  IF sy-subrc = 1.
                    CONCATENATE 'Lote ' vbrp-charg 'bloqueado por ' sperr_user INTO msg SEPARATED BY space.
                    MESSAGE msg TYPE 'E'.
                    EXIT.
                  ENDIF.

                  CALL FUNCTION 'ENQUEUE_EMMCH1E'
                    EXPORTING
                      mode_mch1      = 'E'
                      mandt          = sy-mandt
                      matnr          = wa_zppt0017-matnr_rv
                      charg          = vbrp-charg
                      _scope         = '2'
                    EXCEPTIONS
                      foreign_lock   = 1
                      system_failure = 2
                      OTHERS         = 3.

                  sperr_user  = sy-msgv1.

                  IF sy-subrc = 1.
                    CONCATENATE 'Lote ' vbrp-charg 'bloqueado por ' sperr_user INTO msg SEPARATED BY space.
                    MESSAGE msg TYPE 'E'.
                    EXIT.
                  ENDIF.

                ENDIF.

                REFRESH tl_return.
                "iniciando a bapi

                CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
                  EXPORTING
                    materialdocument = wa_mseg-mblnr
                    matdocumentyear  = wa_mseg-mjahr
                  IMPORTING
                    goodsmvt_headret = wa_head_ret
                  TABLES
                    return           = tl_return.

                READ TABLE tl_return INTO sl_return WITH KEY type = 'E'.
                IF sy-subrc NE 0.
                  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                    EXPORTING
                      wait = 'X'.
                  CLEAR sl_return.
                ELSE.
                  MESSAGE sl_return-message TYPE 'E'.
                  EXIT.
                ENDIF.

                IF fg_bloqueio = 'X'.
                  CALL FUNCTION 'DEQUEUE_EMMARCE'
                    EXPORTING
                      matnr = wa_zppt0017-matnr_ac
                      werks = wa_zppt0017-werks.

                  CALL FUNCTION 'DEQUEUE_EMMARCE'
                    EXPORTING
                      matnr = wa_zppt0017-matnr_rv
                      werks = wa_zppt0017-werks.

                  IF sl_item-batch IS NOT INITIAL.

                    CALL FUNCTION 'DEQUEUE_EMMCH1E'
                      EXPORTING
                        mode_mch1 = 'E'
                        mandt     = sy-mandt
                        matnr     = wa_zppt0017-matnr_ac
                        charg     = vbrp-charg.


                    CALL FUNCTION 'DEQUEUE_EMMCH1E'
                      EXPORTING
                        mode_mch1 = 'E'
                        mandt     = sy-mandt
                        matnr     = wa_zppt0017-matnr_rv
                        charg     = vbrp-charg.
                  ENDIF.
                ENDIF.
                EXPORT: vbap-vbeln TO MEMORY ID 'VDOC'.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.   "<<RIM-SKM-CS1025614-15.09.22
      ENDIF.
    ENDIF.

ENDCASE.
************************************************************************************************

SELECT SINGLE *
  FROM setleaf INTO @DATA(lwa_zles0136_api)
 WHERE setname EQ 'ZLES0136_CONFIG'
   AND valfrom EQ 'DISABLE_CK_0001'.

IF sy-subrc NE 0.

  CASE sy-tcode.
    WHEN 'VF01' OR 'ZLES0136'.

      IF ( vbrk-vbeln IS INITIAL ) AND ( vbrk-sfakn IS INITIAL ).

        SELECT SINGLE *
          FROM zsdt0023
          INTO @DATA(lwa_zsdt0023)
         WHERE vbeln EQ @likp-vbeln.

        IF sy-subrc IS INITIAL.

          IF lwa_zsdt0023-mblnr_e IS NOT INITIAL.
            SELECT SINGLE COUNT(*) INTO @DATA(lv_count) FROM mseg WHERE smbln EQ @lwa_zsdt0023-mblnr_e.
            IF lv_count > 0.
              CONCATENATE 'Remessa:' lwa_zsdt0023-vbeln
                          'esta em processo de estorno ( documento:' lwa_zsdt0023-mblnr_e ' já foi estornado ). Estornar e gerar novamente!' INTO lva_msg_erro_vf SEPARATED BY space.                  MESSAGE lva_msg_erro_vf TYPE 'E'.
            ENDIF.
          ENDIF.

          IF lwa_zsdt0023-mblnr_s IS NOT INITIAL.
            SELECT SINGLE COUNT(*) INTO lv_count FROM mseg WHERE smbln EQ lwa_zsdt0023-mblnr_s.
            IF lv_count > 0.
              CONCATENATE 'Remessa:' lwa_zsdt0023-vbeln
                          'esta em processo de estorno ( documento:' lwa_zsdt0023-mblnr_s ' já foi estornado ). Estornar e gerar novamente!' INTO lva_msg_erro_vf SEPARATED BY space.
              MESSAGE lva_msg_erro_vf TYPE 'E'.
            ENDIF.
          ENDIF.

        ENDIF.

        SELECT SINGLE *
          FROM zsdt0001 INTO @DATA(lwa_romaneio)
         WHERE doc_rem EQ @likp-vbeln.
        "Não checar processos de agrupamento sul
        IF ( sy-subrc EQ 0 ) AND ( lwa_romaneio-tp_movimento = 'S' ) AND ( lwa_romaneio-matnr IS NOT INITIAL ) AND ( lwa_romaneio-peso_liq_pos_ret IS INITIAL ).

          CALL METHOD zcl_romaneio=>get_ck_faturar
            EXPORTING
              i_ch_referencia_sai = lwa_romaneio-ch_referencia
            IMPORTING
              e_romaneios         = DATA(lit_romaneios).

          LOOP AT lit_romaneios INTO DATA(lwa_romaneios).

            CHECK ( lwa_romaneios-peso_subtotal IS NOT INITIAL ) AND ( lwa_romaneios-peso_liq IS NOT INITIAL ).

            SELECT SINGLE *
              FROM likp INTO @DATA(lw_likp_rom)
             WHERE vbeln EQ @lwa_romaneios-doc_rem.

            IF ( sy-subrc EQ 0 ).

              IF ( lw_likp_rom-btgew NE lwa_romaneios-peso_subtotal ).
                WRITE: lw_likp_rom-btgew          TO lva_valor_s_aux_01,
                       lwa_romaneios-peso_subtotal TO lva_valor_s_aux_02.

                CONDENSE: lva_valor_s_aux_01, lva_valor_s_aux_02 NO-GAPS.

                CONCATENATE 'Romaneio:' lwa_romaneios-nr_romaneio 'Remessa:' lwa_romaneios-doc_rem
                            'com peso bruto:' lva_valor_s_aux_01 ' divergente do romaneio:' lva_valor_s_aux_02
                            '! Estornar e gerar novamente!' INTO lva_msg_erro_vf SEPARATED BY space.
                MESSAGE lva_msg_erro_vf TYPE 'E'.
              ENDIF.

              IF lwa_romaneios-peso_liq_pos_ret IS NOT INITIAL.

                IF ( lw_likp_rom-ntgew NE lwa_romaneios-peso_liq_pos_ret ).

                  WRITE: lw_likp_rom-ntgew              TO lva_valor_s_aux_01,
                         lwa_romaneios-peso_liq_pos_ret TO lva_valor_s_aux_02.

                  CONDENSE: lva_valor_s_aux_01, lva_valor_s_aux_02 NO-GAPS.

                  CONCATENATE 'Romaneio:' lwa_romaneios-nr_romaneio 'Remessa:' lwa_romaneios-doc_rem
                              'com peso liquido:' lva_valor_s_aux_01 ' divergente do romaneio:' lva_valor_s_aux_02
                              '! Estornar e gerar novamente!' INTO lva_msg_erro_vf SEPARATED BY space.
                  MESSAGE lva_msg_erro_vf TYPE 'E'.
                ENDIF.

              ELSE.

                IF ( lw_likp_rom-ntgew NE lwa_romaneios-peso_liq ).

                  WRITE: lw_likp_rom-ntgew     TO lva_valor_s_aux_01,
                         lwa_romaneios-peso_liq TO lva_valor_s_aux_02.

                  CONDENSE: lva_valor_s_aux_01, lva_valor_s_aux_02 NO-GAPS.

                  CONCATENATE 'Romaneio:' lwa_romaneios-nr_romaneio 'Remessa:' lwa_romaneios-doc_rem
                              'com peso liquido:' lva_valor_s_aux_01 ' divergente do romaneio:' lva_valor_s_aux_02
                              '! Estornar e gerar novamente!' INTO lva_msg_erro_vf SEPARATED BY space.
                  MESSAGE lva_msg_erro_vf TYPE 'E'.
                ENDIF.

              ENDIF.

            ENDIF.

          ENDLOOP.

        ENDIF.

      ENDIF.

  ENDCASE.

ENDIF.

*&----------------------------------------------------------------------------
*&    Inicio ajuste USER STORY 138113/CS2024000320 / AOENNING / Compara o valor unt da price da OV se esta igual a da fatura que esta sendo gerada.
*&----------------------------------------------------------------------------
* IF sy-tcode EQ 'VF01' AND SY-UCOMM EQ 'SICH' OR SY-TCODE EQ 'ZLES0136' AND SY-UCOMM EQ 'OPT2'.
*      SELECT SINGLE *
*      FROM PRCD_ELEMENTS INTO @DATA(LW_PRCD_ELEMENTS)
*      WHERE KNUMV EQ @YKONVC-KNUMV
*       AND KPOSN EQ @YKONVC-KPOSN
*        AND KSCHL EQ 'ICMI'.
*
*    SELECT SINGLE *
*      FROM PRCD_ELEMENTS INTO @DATA(LS_PRCD_ELEMENTS)
*      WHERE KNUMV EQ @VBAP-KNUMV_ANA
*        AND KPOSN EQ @VBAP-POSNR
*        AND KSCHL EQ 'ICMI'.
*
*   IF LW_PRCD_ELEMENTS-KBETR > 0
*     AND LS_PRCD_ELEMENTS-KBETR > 0
*     AND LW_PRCD_ELEMENTS-WAERS EQ LS_PRCD_ELEMENTS-WAERS.
*
*     IF LW_PRCD_ELEMENTS-KBETR NE LS_PRCD_ELEMENTS-KBETR.
*      CONCATENATE 'O valor unt da condição ICM1 da OV:' VBAP-VBELN
*                              'é diferente do valor unt da condição ICM1 da fatura!' INTO lva_msg_erro_vf SEPARATED BY space.
*                  MESSAGE lva_msg_erro_vf TYPE 'E'.
*
*     ENDIF.
*   ENDIF.
* ENDIF.
*&----------------------------------------------------------------------------
*&    Fim ajuste USER STORY 138113/CS2024000320 / AOENNING.
*&----------------------------------------------------------------------------








ENDENHANCEMENT.
