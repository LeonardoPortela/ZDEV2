*----------------------------------------------------------------------*
*                             AMAGGI                                   *
*----------------------------------------------------------------------*
* Cliente    : Grupo Andre Maggi                                       *
* Autor      : BBKO Consulting S.A.                                    *
* Data       : 08/09/2010                                              *
* Descrição  :                                                         *
* Transação  :                                                         *
* Projeto    : Projeto Evoluir                                         *
* Cód Espec. :                                                         *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
* Em:        | Por:         | Alteração:                               *
*------------+--------------+------------------------------------------*
* 30/09/2010 | Mateus       | Desenvolvimento inicial                  *
*----------------------------------------------------------------------*

REPORT  zlesr0013 MESSAGE-ID zles.

TABLES vttk.


TYPES: BEGIN OF type_msg,
         tp_msn     TYPE bapi_mtype,
         transporte TYPE vttk-tknum,
         doc_custo  TYPE vfkp-fknum,
         messagem   TYPE bapi_msg,
       END   OF type_msg.

CONSTANTS: c_vi01     TYPE sy-tcode       VALUE 'VI01',
           c_n(1)     TYPE c              VALUE 'N',
           c_x(1)     TYPE c              VALUE 'X',
           c_e(1)     TYPE c              VALUE 'E',
           c_s(1)     TYPE c              VALUE 'S',
           c_w(1)     TYPE c              VALUE 'W',
           c_vy(2)    TYPE c              VALUE 'VY',
           c_00(2)    TYPE c              VALUE '00',
           c_01       TYPE inri-nrrangenr VALUE '01',
           c_007(3)   TYPE c              VALUE '007',
           c_344(3)   TYPE c              VALUE '344',
           c_zles0008 TYPE inri-object    VALUE 'ZLES0008'.

CONSTANTS:
  c_f(1)   TYPE c VALUE 'F',
  c_tra(4) TYPE c VALUE 'ZTRA',
  c_lr(2)  TYPE c VALUE 'LR'.

TYPES: BEGIN OF ty_ret,
         message TYPE bapi_msg.
TYPES: END OF ty_ret.

DATA: wa_msg          TYPE          bdcmsgcoll,
      wa_bdc          TYPE          bdcdata,
      wa_zlest0008    TYPE          zlest0008,
      vl_fknum106     TYPE          zsdt0001-fknum,
      vl_ov_frete     TYPE          zsdt0001-ov_frete,
      vl_fatura_frete TYPE          zsdt0001-fatura_frete,
      vl_vbeln        TYPE          vbak-vbeln,
      vl_fat          TYPE          vbak-vbeln,
      vl_fknum        TYPE          vfkp-fknum,
      vlr_zseg        TYPE          konv-kwert,
      vlr_ziof        TYPE          konv-kwert,
      perc_ziof       TYPE          konv-kbetr,
      perc_zseg       TYPE          konv-kbetr,
      vl_message      TYPE          char600,
      vl_tabix        TYPE          sy-tabix,
      vl_mode         TYPE          c,
      vl_cont(10)     TYPE          c,
      wa_return       TYPE          bapiret2,
      tl_return       TYPE          bapiret2_t,
      tl_ret          TYPE TABLE OF ty_ret,
      wa_ret          TYPE          ty_ret,
      zcheck_erro     TYPE          char01, "Verificar erro.
      vl_ponteiro     TYPE          i,
      ti_a916         TYPE TABLE OF a916       WITH HEADER LINE,
      ti_a909         TYPE TABLE OF a909       WITH HEADER LINE,
      ti_konp         TYPE TABLE OF konp       WITH HEADER LINE,
      ti_tvtk         TYPE TABLE OF tvtk       WITH HEADER LINE,
      ti_tvftk        TYPE TABLE OF tvftk      WITH HEADER LINE,
      ti_bdc          TYPE TABLE OF bdcdata    WITH HEADER LINE,
      ti_msg          TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
      ti_zlest0006    TYPE TABLE OF zlest0006  WITH HEADER LINE,
      ti_zlest0008    TYPE TABLE OF zlest0008  WITH HEADER LINE,
      ti_vttk         TYPE TABLE OF vttk       WITH HEADER LINE,
      ti_vtpa         TYPE TABLE OF vtpa       WITH HEADER LINE,
      t_msg           TYPE TABLE OF type_msg.


DATA: it_konv  TYPE TABLE OF konv,
      vl_knumv TYPE          vfkp-knumv.

DATA: vg_pc_veiculo TYPE zplaca,
      e_msg_erro    TYPE bapiret2,
      e_status      TYPE syst_subrc.

*-#133089-21.02.2024-JT-inicio
DATA: lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico,
      vg_faturamento_autom      TYPE char01.
*-#133089-12.02.2024-JT-fim

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.

  SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME TITLE TEXT-002.
    SELECT-OPTIONS:
      so_tknum FOR vttk-tknum OBLIGATORY.
  SELECTION-SCREEN END   OF BLOCK a2.

  SELECTION-SCREEN BEGIN OF BLOCK a3 WITH FRAME TITLE TEXT-003.
    PARAMETERS:
      p_chave  TYPE zsdt0001-ch_referencia NO-DISPLAY,
      p_ebeln  TYPE zlest0108-ebeln NO-DISPLAY,
      p_vbeln  TYPE zlest0108-vbeln NO-DISPLAY,
      rb_in    AS CHECKBOX DEFAULT 'X',
      rb_out   AS CHECKBOX DEFAULT 'X',
      rb_cus   TYPE c NO-DISPLAY DEFAULT '',
      rb_dtfat TYPE sy-datum NO-DISPLAY,
      rb_estvi AS CHECKBOX DEFAULT '' MODIF ID evi.

  SELECTION-SCREEN END   OF BLOCK a3.
SELECTION-SCREEN END   OF BLOCK a1.

PARAMETERS: cksetap TYPE c LENGTH 1 NO-DISPLAY,
            ckrom   TYPE c LENGTH 1 NO-DISPLAY,
            ckfprop TYPE c LENGTH 1 NO-DISPLAY. "Frota Propria

*-#133089-21.02.2024-JT-inicio
PARAMETERS: p_fataut TYPE char01 NO-DISPLAY.
*-#133089-21.02.2024-JT-fim

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    CASE screen-group1.
      WHEN 'EVI'. "SO_ESTVI-LOW

        SELECT SINGLE *
          FROM setleaf INTO @DATA(lwa_set_zles0041_estorno_vi)
         WHERE setname EQ 'ZLES0041_ESTORNO_VI'
           AND valfrom EQ @sy-uname.

        IF sy-subrc EQ 0.
          screen-active = '1'.
        ELSE.
          screen-active = '0'.
        ENDIF.
        MODIFY SCREEN.

    ENDCASE.

  ENDLOOP.


START-OF-SELECTION.

  IF ( sy-tcode EQ 'ZLES0041' ) AND
     ( rb_estvi EQ abap_true  ). "Estorno VI
    PERFORM estorno_custo.
    RETURN.
  ENDIF.

*-#133089-21.02.2024-JT-inicio
*-----------------------------------------------
*-verifica se é faturamento automatico
*-----------------------------------------------
  IF p_fataut = abap_true.
    vg_faturamento_autom = p_fataut.
    CREATE OBJECT lc_faturamento_automatico.
  ENDIF.
*-#133089-21.02.2024-JT-fim

  CLEAR: vl_fknum106, vl_ov_frete,vl_fatura_frete.
  IF sy-tcode EQ 'ZLES0106' OR
     sy-tcode EQ 'ZLES0115' OR
     sy-tcode EQ 'ZLES0136' OR
     sy-tcode EQ 'ZMM0127'  OR
     sy-tcode EQ 'ZLES0113' OR
*-CS2021001045 - 03.02.2022 - JT - inicio
     sy-tcode EQ 'ZLES0200' OR
*-CS2021001045 - 03.02.2022 - JT - fim
     sy-tcode EQ 'ZNFE'     OR "*-CS2024000086-26.09.2024-#151423-JT-inicio
     cksetap  EQ abap_true  OR
     ckrom    EQ abap_true  OR
     p_fataut EQ abap_true.   "*-#133089-21.02.2024-JT
    SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD vl_fknum106.
    SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_ov_frete.
    SET PARAMETER ID 'Z_MY_PARAMETER_3' FIELD vl_fatura_frete.
  ENDIF.

*  IF SY-TCODE = 'ZLES0113'.
*    SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD VL_FKNUM106.
*    SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD VL_OV_FRETE.
*    SET PARAMETER ID 'Z_MY_PARAMETER_3' FIELD VL_FATURA_FRETE.
*  ENDIF.

* Valida documentos de transporte
  PERFORM validar_doc_trp.

* Determinação documento de custo
  PERFORM gerar_custo.

* Determinação ordem de serviço e fatura de serviço.
  PERFORM proc_vda_serv.

* Controle de mensagem.
  PERFORM controle_msg.

END-OF-SELECTION.

  IF NOT t_msg[] IS INITIAL
     AND ( sy-tcode NE 'ZLES0106'  )
     AND ( sy-tcode NE 'ZLES0113'  )
     AND ( sy-tcode NE 'ZLES0115'  )
     AND ( sy-tcode NE 'ZLES0136'  )
*-CS2021001045 - 03.02.2022 - JT - inicio
     AND ( sy-tcode NE 'ZLES0200'  )
*-CS2021001045 - 03.02.2022 - JT - fim
     AND ( sy-tcode NE 'ZNFE'      )  "*-CS2024000086-26.09.2024-#151423-JT-inicio
     AND ( sy-tcode NE 'ZMM0127'   )
     AND ( cksetap  NE abap_true   )
     AND ( ckrom    NE abap_true   )
     AND ( p_fataut EQ abap_false  ).   "*-#133089-21.02.2024-JT
*   Exibe Menssagens
    PERFORM z_exibe_erros.
  ENDIF.

*-#133089-21.02.2024-JT-inicio
  IF p_fataut EQ abap_true.
    LOOP AT t_msg INTO DATA(ww_msg) WHERE tp_msn = 'E'.
      DATA(l_mesg) = ww_msg-messagem.
      lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = p_chave i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'DCUS' ).
    ENDLOOP.
  ENDIF.
*-#133089-21.02.2024-JT-fim

*&---------------------------------------------------------------------*
*&      Form  MAPA_VI01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mapa_vi01 USING p_tknum.

  DATA vl_transbordo TYPE c VALUE space.

  CHECK rb_in = c_x.

  REFRESH: ti_bdc, ti_msg.

  SORT: ti_vtpa  BY vbeln                                            ASCENDING,
        ti_a916  BY kappl kschl shtyp wty_v_parvw wty_v_parnr        ASCENDING,
        ti_a909  BY kappl kschl shtyp wty_v_parvw wty_v_parnr add04  ASCENDING,
        ti_konp  BY knumh ASCENDING,
        ti_vttk  BY tknum,
        ti_tvtk  BY shtyp,
        ti_tvftk BY fkart.

  READ TABLE ti_vttk
    WITH KEY tknum = p_tknum
    BINARY SEARCH.

  READ TABLE ti_tvtk
    WITH KEY shtyp = ti_vttk-shtyp
    BINARY SEARCH.

  READ TABLE ti_tvftk
    WITH KEY fkart = ti_tvtk-fkart
    BINARY SEARCH.

  IF sy-subrc IS INITIAL.

*   Parceiro LR "Local de entrega
    READ TABLE ti_vtpa
      WITH KEY vbeln = p_tknum
      BINARY SEARCH.

    IF sy-subrc IS INITIAL.
*     Sequencia de acesso 1
      READ TABLE ti_a916 WITH KEY kappl          = c_f
                                  kschl          = c_tra
                                  shtyp          = ti_vttk-shtyp
                                  wty_v_parvw    = c_lr
                                  wty_v_parnr    = ti_vtpa-kunnr BINARY SEARCH.

      IF sy-subrc IS INITIAL.

        READ TABLE ti_konp
          WITH KEY knumh = ti_a916-knumh
          BINARY SEARCH.

        IF sy-subrc IS INITIAL AND ti_konp-kbetr GT 0.
          vl_transbordo = c_x.
        ELSE.
          CLEAR vl_transbordo.
        ENDIF.

      ELSE.
*       Sequencia de acesso 2
        READ TABLE ti_a909
          WITH KEY kappl       = c_f
                   kschl       = c_tra
                   shtyp       = ti_vttk-shtyp
                   wty_v_parvw = c_lr
                   wty_v_parnr = ti_vtpa-kunnr
                   add04       = ti_vttk-add04
          BINARY SEARCH.

        IF sy-subrc IS INITIAL.

          READ TABLE ti_konp
            WITH KEY knumh = ti_a909-knumh
            BINARY SEARCH.

          IF sy-subrc IS INITIAL AND ti_konp-kbetr > 0.
            vl_transbordo = c_x.
          ELSE.
            CLEAR vl_transbordo.
          ENDIF.

        ELSE.
          CLEAR vl_transbordo.
        ENDIF.

      ENDIF.

    ELSE.
      CLEAR vl_transbordo.
    ENDIF.

  ELSE.
    CLEAR vl_transbordo.
  ENDIF.


  "" VERIFICA PERMISSÃO DO USUÁRIO COM RELAÇÃO A DATA RETROATIVA
  "" AJUSTE POR ERRO (06/11/2014) DE BACKUP DO BANCO DB2
  DATA: wa_setleaf TYPE setleaf,
        vl_data    TYPE c LENGTH 10.
  CLEAR: wa_setleaf.
  SELECT SINGLE *
    FROM setleaf
    INTO wa_setleaf
   WHERE setname = 'VF01_USUARIO'
     AND valfrom = sy-uname.

  IF sy-subrc IS INITIAL.
    IF rb_dtfat IS NOT INITIAL.
      CONCATENATE rb_dtfat+6(2) '.' rb_dtfat+4(2) '.' rb_dtfat(4) INTO vl_data.
    ELSE.
      PERFORM memorizar_dt_movimento_badi USING ti_vttk-datbg.
      CONCATENATE ti_vttk-datbg+6(2) '.' ti_vttk-datbg+4(2) '.' ti_vttk-datbg(4) INTO vl_data.
    ENDIF.
  ENDIF.

  IF vl_transbordo IS INITIAL.

    IF wa_setleaf IS NOT INITIAL.
      PERFORM zf_bdc USING: 'X' 'SAPMV54A'   '0010',
                            ' ' 'BDC_CURSOR' 'VTTK-TKNUM',
                            ' ' 'BDC_OKCODE' '=UEBP',
                            ' ' 'VTTK-TKNUM'  p_tknum,
                            ' ' 'VFKK-BUDAT'  vl_data.
    ELSE.
      PERFORM zf_bdc USING: 'X' 'SAPMV54A'   '0010',
                            ' ' 'BDC_CURSOR' 'VTTK-TKNUM',
                            ' ' 'BDC_OKCODE' '=UEBP',
                            ' ' 'VTTK-TKNUM'  p_tknum.
    ENDIF.

    PERFORM zf_bdc USING: 'X' 'SAPMV54A'   '0030',
                          ' ' 'BDC_CURSOR' 'VFKK-FKNUM',
                          ' ' 'BDC_OKCODE' '=SICH',
                          ' ' 'BDC_SUBSCR' 'SAPMV54A'.

  ELSE.

    IF wa_setleaf IS NOT INITIAL.
      PERFORM zf_bdc USING: 'X' 'SAPMV54A'       '0010',
                            ' ' 'BDC_CURSOR'     'VTTK-TKNUM',
                            ' ' 'BDC_OKCODE'     '=UEBP',
                            ' ' 'VTTK-TKNUM'     p_tknum,
                            ' ' 'VFKK-BUDAT'     vl_data.
    ELSE.
      PERFORM zf_bdc USING: 'X' 'SAPMV54A'       '0010',
                            ' ' 'BDC_CURSOR'     'VTTK-TKNUM',
                            ' ' 'BDC_OKCODE'     '=UEBP',
                            ' ' 'VTTK-TKNUM'     p_tknum.
    ENDIF.

    PERFORM zf_bdc USING: 'X' 'SAPMV54A'       '0030',
                          ' ' 'BDC_CURSOR'     'VFKP-FKPOS(02)',
                          ' ' 'BDC_OKCODE'     '=PLOE',
                          ' ' 'VIM_MARKED(02)' 'X',
                          ' ' 'BDC_SUBSCR'     'SAPMV54A                                0031SCD_HEADER'.

    PERFORM zf_bdc USING: 'X' 'SAPLSPO1'       '0100',
                          ' ' 'BDC_OKCODE'     '=YES'.

    PERFORM zf_bdc USING: 'X' 'SAPMV54A'   '0030',
                          ' ' 'BDC_CURSOR' 'VFKK-FKNUM',
                          ' ' 'BDC_OKCODE' '=SICH',
                          ' ' 'BDC_SUBSCR' 'SAPMV54A                                0031SCD_HEADER'.

  ENDIF.

ENDFORM.                                                    " MAPA_VI01

*&---------------------------------------------------------------------*
*&      Form  PROC_CUSTO_FRETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM proc_custo_frete USING p_tknum TYPE vttk-tknum.
  DATA: v_erro TYPE char01.
  CHECK rb_in = c_x.

  CLEAR: vl_cont, vl_fknum, wa_zlest0008.

  vl_mode = c_n.

  CALL TRANSACTION c_vi01
    USING ti_bdc
    MODE   vl_mode
    UPDATE c_s
    MESSAGES INTO ti_msg.

*-----------------------------------------------------------------------------------*
* Gravar Logs
*-----------------------------------------------------------------------------------*

  SELECT SINGLE *
    FROM setleaf INTO @DATA(lwa_log_zles0041)
   WHERE setname EQ 'ZLES0041_LOGS'
     AND valfrom EQ 'VI'.

  IF sy-subrc EQ 0.

    DATA: lva_zlest0100_l1 TYPE zlest0100_l1,
          lva_seq_log      TYPE zlest0100_l1-seq.

    CLEAR: lva_seq_log.
    LOOP AT ti_msg INTO wa_msg.

      CLEAR: lva_zlest0100_l1.

      ADD 1 TO lva_seq_log.

      lva_zlest0100_l1-id_referencia  = 'VI_TKNUM_' && p_tknum.
      lva_zlest0100_l1-seq            = lva_seq_log.
      lva_zlest0100_l1-dyname         = wa_msg-dyname.
      lva_zlest0100_l1-dynumb         = wa_msg-dynumb.
      lva_zlest0100_l1-fldname        = wa_msg-fldname.
      lva_zlest0100_l1-msgtyp         = wa_msg-msgtyp.
      lva_zlest0100_l1-msgspra        = wa_msg-msgspra.
      lva_zlest0100_l1-msgid          = wa_msg-msgid.
      lva_zlest0100_l1-msgnr          = wa_msg-msgnr.
      lva_zlest0100_l1-msgv1          = wa_msg-msgv1.
      lva_zlest0100_l1-msgv2          = wa_msg-msgv2.
      lva_zlest0100_l1-msgv3          = wa_msg-msgv3.
      lva_zlest0100_l1-msgv4          = wa_msg-msgv4.
      lva_zlest0100_l1-msgv4          = wa_msg-msgv4.
      lva_zlest0100_l1-dt_registro    = sy-datum.
      lva_zlest0100_l1-hr_registro    = sy-uzeit.
      lva_zlest0100_l1-us_registro    = sy-uname.

      MODIFY zlest0100_l1 FROM lva_zlest0100_l1.

    ENDLOOP.
  ENDIF.

*-----------------------------------------------------------------------------------*
* Gravar Logs Fim
*-----------------------------------------------------------------------------------*

  READ TABLE ti_msg INTO wa_msg
    WITH KEY msgtyp = c_s
             msgid  = c_vy
             msgnr  = c_007.

  IF sy-subrc EQ 0.

    MESSAGE ID     wa_msg-msgid
            TYPE   wa_msg-msgtyp
            NUMBER wa_msg-msgnr
            WITH   wa_msg-msgv1 wa_msg-msgv2 wa_msg-msgv3 wa_msg-msgv4
            INTO   vl_message.

    wa_zlest0008-mandt      = sy-mandt.
    wa_zlest0008-filename   = sy-cprog.
    wa_zlest0008-tcode      = sy-tcode.
    wa_zlest0008-cont       = vl_ponteiro.
    wa_zlest0008-msgtyp     = wa_msg-msgtyp.
    wa_zlest0008-msgspra    = sy-langu.
    wa_zlest0008-msgid      = wa_msg-msgid.
    wa_zlest0008-msgnr      = wa_msg-msgnr.
    wa_zlest0008-msgv1      = vl_message.
    wa_zlest0008-data       = sy-datum.
    wa_zlest0008-hora       = sy-uzeit.
    wa_zlest0008-usuario    = sy-uname.

    APPEND wa_zlest0008 TO ti_zlest0008.

    vl_fknum    = wa_msg-msgv1.
    vl_ponteiro = vl_ponteiro + 1.

    IF ( sy-tcode NE 'ZLES0106' ) AND
       ( sy-tcode NE 'ZLES0113' ) AND
       ( sy-tcode NE 'ZLES0115' ) AND
       ( sy-tcode NE 'ZLES0136' ) AND
*-CS2021001045 - 03.02.2022 - JT - inicio
       ( sy-tcode NE 'ZLES0200' ) AND
*-CS2021001045 - 03.02.2022 - JT - fim
       ( sy-tcode NE 'ZMM0127'  ) AND
       ( sy-tcode NE 'ZNFE'     ) AND "*-CS2024000086-26.09.2024-#151423-JT-inicio
       ( cksetap  NE abap_true  ) AND
       ( p_fataut EQ abap_false ) AND   "*-#133089-21.02.2024-JT
       ( ckrom    NE abap_true ).
*   Monta Mensagem
      PERFORM z_monta_erro USING p_tknum
                                 vl_fknum
                                 'S'
                                 vl_message.
    ELSE.
      SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD vl_fknum.

      IF p_chave IS NOT INITIAL.
        IF  sy-tcode EQ 'ZLES0115'.
          UPDATE zsdt0001 SET st_proc      = '15' " Doc.Custo
                      fknum        = vl_fknum
          WHERE ch_referencia = p_chave.
        ELSE.
          UPDATE zsdt0001 SET st_proc      = '05' " Doc.Custo
                              fknum        = vl_fknum
          WHERE ch_referencia = p_chave.
        ENDIF.
      ENDIF.

      IF p_vbeln IS NOT INITIAL.
        IF  sy-tcode EQ 'ZLES0115'.
          UPDATE zlest0108 SET st_proc = '15' " Doc.Custo
                               fknum   = vl_fknum
          WHERE vbeln = p_vbeln
            AND ebeln = p_ebeln.
        ELSE.
          UPDATE zlest0108 SET st_proc = '05' " Doc.Custo
                               fknum   = vl_fknum
          WHERE vbeln = p_vbeln
            AND ebeln = p_ebeln.
        ENDIF.

      ENDIF.


    ENDIF.

  ELSE.
    "===================================================USER STORY 61743 / Anderson Oenning
    CLEAR: v_erro.
    READ TABLE ti_msg INTO wa_msg WITH KEY msgtyp = 'I' msgid  = 'ZLES' msgnr  = '137'.
    IF sy-subrc EQ 0.
      v_erro = 'X'.
    ENDIF.

    READ TABLE ti_msg INTO wa_msg WITH KEY msgtyp = 'I' msgid  = 'ZLES' msgnr  = '152'.
    IF sy-subrc EQ 0.
      v_erro = 'X'.
    ENDIF.

    IF v_erro IS NOT INITIAL AND sy-batch EQ abap_false.
*    IF sy-subrc IS INITIAL AND sy-batch EQ abap_false.
      MESSAGE ID wa_msg-msgid TYPE wa_msg-msgtyp NUMBER wa_msg-msgnr WITH wa_msg-msgv1 wa_msg-msgv2 wa_msg-msgv3 wa_msg-msgv4 DISPLAY LIKE 'E'.
    ELSEIF v_erro IS NOT INITIAL AND sy-batch EQ abap_true.
*    ELSEIF sy-subrc IS INITIAL  AND sy-batch EQ abap_true.
      MESSAGE ID wa_msg-msgid TYPE 'S' NUMBER wa_msg-msgnr WITH wa_msg-msgv1 wa_msg-msgv2 wa_msg-msgv3 wa_msg-msgv4 DISPLAY LIKE 'E'.
    ENDIF.
    "===================================================USER STORY 61743 / Anderson Oenning
    READ TABLE ti_msg INTO wa_msg WITH KEY msgtyp = c_s msgid  = c_00 msgnr  = c_344.

    IF NOT sy-subrc IS INITIAL.
      DELETE ti_msg WHERE msgtyp NE c_e.
    ENDIF.

    LOOP AT ti_msg INTO wa_msg.

      MESSAGE ID     wa_msg-msgid
              TYPE   wa_msg-msgtyp
              NUMBER wa_msg-msgnr
              WITH   wa_msg-msgv1 wa_msg-msgv2 wa_msg-msgv3 wa_msg-msgv4
              INTO   vl_message.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = c_01
          object                  = c_zles0008
        IMPORTING
          number                  = vl_cont
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE c_w NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.

        wa_zlest0008-mandt      = sy-mandt.
        wa_zlest0008-filename   = sy-cprog.
        wa_zlest0008-tcode      = sy-tcode.
        wa_zlest0008-msgtyp     = wa_msg-msgtyp.
        wa_zlest0008-msgspra    = sy-langu.
        wa_zlest0008-msgid      = wa_msg-msgid.
        wa_zlest0008-msgnr      = wa_msg-msgnr.
        wa_zlest0008-msgv1      = vl_message.
        wa_zlest0008-data       = sy-datum.
        wa_zlest0008-hora       = sy-uzeit.
        wa_zlest0008-usuario    = sy-uname.
        wa_zlest0008-cont       = vl_ponteiro.

        APPEND wa_zlest0008 TO ti_zlest0008.

        vl_ponteiro = vl_ponteiro + 1.

*       Monta Mensagem
        PERFORM z_monta_erro USING p_tknum
                                   space
                                   'E'
                                   vl_message.

      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " PROC_CUSTO_FRETE

*&---------------------------------------------------------------------*
*&      Form  zf_bdc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DYNBEGIN text
*      -->P_NAME     text
*      -->P_VALUE    text
*----------------------------------------------------------------------*
FORM zf_bdc USING p_dynbegin TYPE any
                  p_name     TYPE any
                  p_value    TYPE any.

  IF p_dynbegin EQ c_x.

    wa_bdc-program  = p_name.
    wa_bdc-dynpro   = p_value.
    wa_bdc-dynbegin = p_dynbegin.

    APPEND wa_bdc TO ti_bdc.

  ELSE.

    wa_bdc-fnam = p_name.
    wa_bdc-fval = p_value.

    APPEND wa_bdc TO ti_bdc.

  ENDIF.

  CLEAR wa_bdc.

ENDFORM.                    "zf_bdc

*&---------------------------------------------------------------------*
*&      Form  PROC_VDA_SERV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM proc_vda_serv.

  CHECK rb_out EQ c_x.

  CHECK NOT ti_vttk[] IS INITIAL.

  LOOP AT ti_vttk.

    IF ti_vttk-abfer EQ '1'. "Saida - Não foi colocado entrada, pois nessa época ainda tinha um ajuste não testado
      TRY .
          zcl_faturamento=>zif_faturamento~get_instance(
             )->get_processo_emissao_docs(
            EXPORTING
              i_tknum                = ti_vttk-tknum " Nº transporte
            IMPORTING
              e_conhecimento         = DATA(e_conhecimento_1)
            ).

          IF e_conhecimento_1 EQ abap_false.
            CONTINUE.
          ENDIF.
        CATCH zcx_faturamento.    "
        CATCH zcx_error.    " .
      ENDTRY.
    ENDIF.

    CLEAR: vl_vbeln, vl_fknum, vl_message.

*   Verifica se ja foi criado ordem de vendas de serviço para o documento
    SELECT SINGLE vbeln
      FROM vbak
      INTO vl_vbeln
    WHERE  tknum EQ ti_vttk-tknum.

    IF NOT sy-subrc IS INITIAL.
*     Verifica se ha documento de custo de frete para o documento.
      SELECT SINGLE fknum knumv
        FROM vfkp
        INTO ( vl_fknum, vl_knumv )
      WHERE rebel  EQ ti_vttk-tknum
        AND refty  EQ '8'
        AND fkpty  EQ 'Z001'.

*     Criar Ordem e fatura de serviço.
      IF sy-subrc IS INITIAL.

**=============================us #66690 / aoenning / 21-06-2023

*        " seleção para buscar o valor da ZSEG E ZIOF da VI.
*        SELECT * FROM konv
*        INTO TABLE it_konv
*        WHERE knumv EQ vl_knumv
*          AND kschl IN ( 'ZSEG', 'ZIOF' ).
*        SORT it_konv BY kschl.

        " seleção para buscar o valor da ZSEG E ZIOF da VI.
        SELECT * FROM v_konv_cds
        INTO TABLE it_konv
        WHERE knumv EQ vl_knumv
          AND kschl IN ( 'ZSEG', 'ZIOF' ).
        SORT it_konv BY kschl.


        CLEAR: vlr_zseg, vlr_ziof, perc_ziof, perc_zseg.
        LOOP AT it_konv INTO DATA(ws_konv).
          "Selecionar o valor de cada tipo documento.
          CASE ws_konv-kschl.
            WHEN 'ZIOF'.
              vlr_ziof = ws_konv-kwert.
              perc_ziof = ws_konv-kbetr. "Ajuste realizado BUG SOLTO 134201 / IR1740649  / AOENNING.
            WHEN 'ZSEG'.
              vlr_zseg = ws_konv-kwert.
              perc_zseg = ws_konv-kbetr. "Ajuste realizado BUG SOLTO 134201 / IR1740649  / AOENNING.
          ENDCASE.
        ENDLOOP.

        "Placa cavalo.
        CLEAR: vg_pc_veiculo, e_status, zcheck_erro.
        vg_pc_veiculo =  ti_vttk-text1+0(7).

        "Verificar.
        CALL FUNCTION 'Z_LES_EXC_ZSEG'
          EXPORTING
            i_placa       = vg_pc_veiculo
            i_ck_consulta = abap_true
          IMPORTING
            e_msg_erro    = e_msg_erro
            e_status      = e_status.

        "Retorno comunicação status situação do transportador.
        CASE e_status.
          WHEN 1. "Erro de comunicação com a API
            wa_return-message = |{ e_msg_erro-message_v1 } { e_msg_erro-message_v2 } { e_msg_erro-message_v3 }|.
            PERFORM z_monta_erro USING ti_vttk-tknum
                                   vl_fknum
                                   'E'
                                   wa_return-message.
          WHEN 2. "ETC Não equiparado
            IF vlr_ziof EQ 0 AND perc_ziof EQ 0. "Ajuste realizado BUG SOLTO 134201 / IR1740649  / AOENNING.
              zcheck_erro = abap_true.
              wa_return-message = 'VI sem desconto de IOF, estornar e fazer novamente'.
              PERFORM z_monta_erro USING ti_vttk-tknum
                                     vl_fknum
                                     'E'
                                     wa_return-message.
            ELSEIF vlr_zseg EQ 0 AND perc_zseg EQ 0. "Ajuste realizado BUG SOLTO 134201 / IR1740649  / AOENNING.
              zcheck_erro = abap_true.
              wa_return-message = 'VI sem desconto de seguro, estornar e fazer novamente'.
              PERFORM z_monta_erro USING ti_vttk-tknum
                                     vl_fknum
                                     'E'
                                     wa_return-message.
            ENDIF.

          WHEN 3. "Tip não retornou o resultado esperado na consulta
            wa_return-message = |{ e_msg_erro-message_v1 } { e_msg_erro-message_v2 } { e_msg_erro-message_v3 }|.
            PERFORM z_monta_erro USING ti_vttk-tknum
                                   vl_fknum
                                   'E'
                                   wa_return-message.
          WHEN OTHERS.
        ENDCASE.

        "Se houver erro no processo anterior, não seguir para criar a OV, retornar a msg de erro para o usuario.
        IF zcheck_erro EQ abap_true.
          CONTINUE.
        ENDIF.
***==================================US #66690 / AOENNING / 21-06-2023


        CLEAR: vl_vbeln, wa_return.

        CALL FUNCTION 'ZSD_OV_ZTRO'
          EXPORTING
            p_tknum             = ti_vttk-tknum
            p_tdlnr             = ti_vttk-tdlnr
            p_add03             = ti_vttk-add03
            p_sem_mensagem      = cksetap
            p_faturamento_autom = vg_faturamento_autom  "*-#133089-21.02.2024-JT
          IMPORTING
            p_sales             = vl_vbeln
            p_fat               = vl_fat
            t_return            = tl_return.

        IF NOT tl_return[] IS INITIAL.

          LOOP AT tl_return INTO wa_return.

            wa_zlest0008-mandt      = sy-mandt.
            wa_zlest0008-filename   = sy-cprog.
            wa_zlest0008-tcode      = sy-tcode.
            wa_zlest0008-msgtyp     = 'E'.
            wa_zlest0008-msgspra    = sy-langu.
            wa_zlest0008-msgid      = 'SD'.
            wa_zlest0008-msgnr      = '000'.
            wa_zlest0008-msgv1      = wa_return-message.
            wa_zlest0008-data       = sy-datum.
            wa_zlest0008-hora       = sy-uzeit.
            wa_zlest0008-usuario    = sy-uname.
            wa_zlest0008-cont       = vl_ponteiro.

            APPEND wa_zlest0008 TO ti_zlest0008.

            vl_ponteiro = vl_ponteiro + 1.

*           Monta Mensagem
            PERFORM z_monta_erro USING ti_vttk-tknum
                                       vl_fknum
                                       'E'
                                       wa_return-message.
          ENDLOOP.

        ELSE.

          IF NOT vl_vbeln IS INITIAL.
            IF ( sy-tcode EQ 'ZLES0136' ) OR ( sy-tcode EQ 'ZNFE' ) OR "*-CS2024000086-26.09.2024-#151423-JT-inicio
               ( vg_faturamento_autom = abap_true ). "*-#133089-21.02.2024-JT
              SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_vbeln.
              UPDATE zsdt0001 SET st_proc      = '06' " OV_FRETE
                                  ov_frete     = vl_ov_frete
              WHERE ch_referencia = p_chave.
            ENDIF.
            IF ( sy-tcode EQ 'ZLES0106' OR sy-tcode EQ 'ZMM0127' OR ckrom EQ abap_true ).
              SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_vbeln.
              UPDATE zsdt0001 SET st_proc      = '06' " OV_FRETE
                                  ov_frete     = vl_ov_frete
              WHERE ch_referencia = p_chave.
            ENDIF.
            IF sy-tcode EQ 'ZLES0115'.
              SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_vbeln.
              UPDATE zsdt0001 SET st_proc      = '16' " OV_FRETE
                                  ov_frete     = vl_ov_frete
              WHERE ch_referencia = p_chave.
            ENDIF.
            IF sy-tcode EQ 'ZLES0113' OR cksetap EQ abap_true.
              SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_vbeln.

              UPDATE zlest0108 SET st_proc      = '06' " OV_FRETE
                                   ov_frete     = vl_ov_frete
              WHERE vbeln = p_vbeln
                AND ebeln = p_ebeln.
            ENDIF.


            CONCATENATE TEXT-004 vl_vbeln INTO vl_message.
            CONDENSE vl_message NO-GAPS.

            wa_zlest0008-mandt      = sy-mandt.
            wa_zlest0008-filename   = sy-cprog.
            wa_zlest0008-tcode      = sy-tcode.
            wa_zlest0008-msgtyp     = 'S'.
            wa_zlest0008-msgspra    = sy-langu.
            wa_zlest0008-msgid      = 'SD'.
            wa_zlest0008-msgnr      = '004'.
            wa_zlest0008-msgv1      = vl_message.
            wa_zlest0008-data       = sy-datum.
            wa_zlest0008-hora       = sy-uzeit.
            wa_zlest0008-usuario    = sy-uname.
            wa_zlest0008-cont       = vl_ponteiro.

            APPEND wa_zlest0008 TO ti_zlest0008.

            vl_ponteiro = vl_ponteiro + 1.

*           Monta Mensagem
            PERFORM z_monta_erro USING ti_vttk-tknum
                                       vl_fknum
                                       'S'
                                       vl_message.

            IF NOT vl_fat IS INITIAL.

              IF sy-tcode EQ 'ZLES0136' OR vg_faturamento_autom = abap_true. "*-#133089-21.02.2024-JT.
                SET PARAMETER ID 'Z_MY_PARAMETER_3' FIELD vl_fat.
                UPDATE zsdt0001 SET st_proc      = '07' "  " Fatura Frete
                                    fatura_frete = vl_fat
                WHERE ch_referencia = p_chave.
              ENDIF.

              IF sy-tcode EQ 'ZLES0106' OR sy-tcode EQ 'ZMM0127' AND ckrom EQ abap_true.
                SET PARAMETER ID 'Z_MY_PARAMETER_3' FIELD vl_fat.
                UPDATE zsdt0001 SET st_proc      = '07' "  " Fatura Frete
                                    fatura_frete = vl_fat
                WHERE ch_referencia = p_chave.
              ENDIF.

              IF sy-tcode EQ 'ZLES0115'.
                SET PARAMETER ID 'Z_MY_PARAMETER_3' FIELD vl_fat.
                UPDATE zsdt0001 SET st_proc      = '17' "  " Fatura Frete
                                    fatura_frete = vl_fat
                WHERE ch_referencia = p_chave.
              ENDIF.

              IF sy-tcode EQ 'ZLES0113' OR cksetap EQ abap_true.
                SET PARAMETER ID 'Z_MY_PARAMETER_3' FIELD vl_fat.
                UPDATE zlest0108 SET st_proc      = '07' "  " Fatura Frete
                                     fatura_frete = vl_fat
                 WHERE vbeln = p_vbeln
                   AND ebeln = p_ebeln.
              ENDIF.

              CONCATENATE TEXT-004 vl_fat INTO vl_message.
              CONDENSE vl_message NO-GAPS.

              wa_zlest0008-mandt      = sy-mandt.
              wa_zlest0008-filename   = sy-cprog.
              wa_zlest0008-tcode      = sy-tcode.
              wa_zlest0008-msgtyp     = 'S'.
              wa_zlest0008-msgspra    = sy-langu.
              wa_zlest0008-msgid      = 'SD'.
              wa_zlest0008-msgnr      = '005'.
              wa_zlest0008-msgv1      = vl_message.
              wa_zlest0008-data       = sy-datum.
              wa_zlest0008-hora       = sy-uzeit.
              wa_zlest0008-usuario    = sy-uname.
              wa_zlest0008-cont       = vl_ponteiro.

              APPEND wa_zlest0008 TO ti_zlest0008.

              vl_ponteiro = vl_ponteiro + 1.

*             Monta Mensagem
              PERFORM z_monta_erro USING ti_vttk-tknum
                                         vl_fknum
                                         'S'
                                         vl_message.


            ENDIF.

          ENDIF.

        ENDIF.

*-------CS2019001158 - Jaime Tassoni - 16.11.2020 - inicio
        IF sy-tcode = 'ZMM0127' AND tl_return[] IS NOT INITIAL.
          FREE: tl_ret.
          LOOP AT tl_return            INTO wa_return WHERE type = 'E'.
            MOVE-CORRESPONDING wa_return TO wa_ret.
            APPEND wa_ret                TO tl_ret.
          ENDLOOP.

          IF tl_ret[] IS NOT INITIAL AND vg_faturamento_autom = abap_false. "*-#133089-21.02.2024-JT
            CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY_OK'
              EXPORTING
                endpos_col   = 140
                endpos_row   = 15
                startpos_col = 50
                startpos_row = 10
                titletext    = TEXT-101
              TABLES
                valuetab     = tl_ret
              EXCEPTIONS
                break_off    = 1
                OTHERS       = 2.
          ENDIF.
        ENDIF.
*-------CS2019001158 - Jaime Tassoni - 16.11.2020 - fim

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " PROC_VDA_SERV

*&---------------------------------------------------------------------*
*&      Form  GERAR_CUSTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gerar_custo .

  CHECK NOT ti_vttk[] IS INITIAL.

  LOOP AT ti_vttk.

*   Mapa da batch input do documento de custo de frete
    PERFORM mapa_vi01 USING ti_vttk-tknum.

*   Determinação documento de custo
    PERFORM proc_custo_frete USING ti_vttk-tknum.

  ENDLOOP.

ENDFORM.                    " GERAR_CUSTO


*&---------------------------------------------------------------------*
*&      Form  VALIDAR_DOC_TRP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validar_doc_trp .

  DATA: v_msg TYPE c LENGTH 150.

  REFRESH: ti_vttk, ti_vtpa, ti_a909, ti_konp, t_msg.

* Documento de transporte
  IF ( rb_cus = 'X' ) AND ( ckfprop IS INITIAL ).
    SELECT *
    FROM vttk
    INTO TABLE ti_vttk
  WHERE tknum IN so_tknum
    AND add03 EQ  '0000000002'
    AND stabf EQ c_x
    AND datbg NE space.
  ELSE.
    SELECT *
      FROM vttk
      INTO TABLE ti_vttk
    WHERE tknum IN so_tknum
      AND add03 EQ  '0000000001'
      AND stabf EQ c_x
      AND datbg NE space.
  ENDIF.

  IF ti_vttk[] IS INITIAL.
*   Monta Erro
    PERFORM z_monta_erro USING space
                               space
                               'E'
                               TEXT-006.
    EXIT.
  ENDIF.

  LOOP AT ti_vttk.
    TRY.
        zcl_faturamento=>zif_faturamento~get_instance( )->get_processo_emissao_docs(
          EXPORTING
           i_tknum                =   ti_vttk-tknum
         IMPORTING
           e_tipo_veiculo         =   DATA(_tp_veiculo)
           e_conhecimento         =   DATA(_emite_conhecimento) ).

        IF ( rb_out EQ abap_true ) AND ( _tp_veiculo = 'P' ) AND ( _emite_conhecimento = abap_false ).

          CLEAR: ti_vttk[].

          CONCATENATE TEXT-007 TEXT-008 INTO v_msg SEPARATED BY space.
          PERFORM z_monta_erro USING space
                                     space
                                     'E'
                                     v_msg.

          RETURN.
        ENDIF.

      CATCH zcx_faturamento INTO DATA(_cx_fat).

        CLEAR: ti_vttk[].

        v_msg = _cx_fat->zif_error~get_msg_erro( ).

        PERFORM z_monta_erro USING space
                                   space
                                   'E'
                                   v_msg.
        RETURN.

      CATCH zcx_error INTO DATA(_cx_error).

        CLEAR: ti_vttk[].

        v_msg = _cx_error->zif_error~get_msg_erro( ).

        PERFORM z_monta_erro USING space
                                   space
                                   'E'
                                   v_msg.

        RETURN.

    ENDTRY.
  ENDLOOP.

* Relação do documento de custo de frete e documento de transporte
  SELECT *
    FROM tvtk
    INTO TABLE ti_tvtk
    FOR ALL ENTRIES IN ti_vttk
  WHERE shtyp EQ ti_vttk-shtyp.

* Categoria de documento de custo
  SELECT *
    FROM tvftk
    INTO TABLE ti_tvftk
    FOR ALL ENTRIES IN ti_tvtk
  WHERE fkart EQ ti_tvtk-fkart.
*    AND fkpty EQ 'Z003'.
  DELETE ti_tvftk WHERE fkpty NE 'Z001'
                    AND fkpty NE 'Z003'.

* Parceiro LR "Local de entrega
  SELECT *
    FROM  vtpa
    INTO TABLE ti_vtpa
    FOR ALL ENTRIES IN ti_vttk
  WHERE vbeln EQ ti_vttk-tknum
    AND parvw EQ c_lr.

* Preço
  SELECT *
    FROM a916
    INTO TABLE ti_a916
    FOR ALL ENTRIES IN ti_vttk
  WHERE kappl       EQ c_f
    AND kschl       EQ c_tra
    AND shtyp       EQ ti_vttk-shtyp
    AND wty_v_parvw EQ c_lr
    AND datbi       GE sy-datum
    AND datab       LE sy-datum.

  IF NOT ti_a916[] IS  INITIAL.
*   Preço
    SELECT *
      FROM konp
      APPENDING TABLE ti_konp
      FOR ALL ENTRIES IN ti_a916
    WHERE knumh    EQ ti_a916-knumh
      AND loevm_ko NE c_x.
  ENDIF.

* Preço
  SELECT *
    FROM a909
    INTO TABLE ti_a909
    FOR ALL ENTRIES IN ti_vttk
  WHERE kappl       EQ c_f
    AND kschl       EQ c_tra
    AND shtyp       EQ ti_vttk-shtyp
    AND wty_v_parvw EQ c_lr
    AND add04       EQ ti_vttk-add04
    AND datbi       GE sy-datum
    AND datab       LE sy-datum.

  IF  NOT ti_a909[] IS  INITIAL.
*   Preço
    SELECT *
      FROM konp
      APPENDING TABLE ti_konp
      FOR ALL ENTRIES IN ti_a909
    WHERE knumh    EQ ti_a909-knumh
      AND loevm_ko LE c_x.
  ENDIF.

ENDFORM.                    " VALIDAR_DOC_TRP


*&---------------------------------------------------------------------*
*&      Form  CONTROLE_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM controle_msg .

  MODIFY zlest0008 FROM TABLE ti_zlest0008.

ENDFORM.                    " CONTROLE_MSG

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_ERRO                                             *
*&---------------------------------------------------------------------*
*                               Monta Erro                             *
*----------------------------------------------------------------------*
FORM z_monta_erro USING p_tknum   TYPE c
                        p_fknum   TYPE c
                        p_type    TYPE c
                        p_message TYPE c.

  DATA sl_msg TYPE type_msg.

  sl_msg-messagem   = p_message.
  sl_msg-tp_msn     = p_type.
  sl_msg-transporte = p_tknum.
  sl_msg-doc_custo  = p_fknum.

  APPEND sl_msg TO t_msg.

ENDFORM.                    " Z_MONTA_ERRO

*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_ERROS                                            *
*&---------------------------------------------------------------------*
*                            Exibe Mensagens                           *
*----------------------------------------------------------------------*
FORM z_exibe_erros.

  CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
    TABLES
      table    = t_msg
    EXCEPTIONS
      fb_error = 1
      OTHERS   = 2.

ENDFORM.                    " Z_EXIBE_ERROS

*&---------------------------------------------------------------------*
*&      Form  MEMORIZAR_DT_MOVIMENTO_BADI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TI_VTTK_DPLBG  text
*----------------------------------------------------------------------*
FORM memorizar_dt_movimento_badi  USING p_data_rem TYPE ledat.

  TYPES:
    BEGIN OF tab_type,
      para TYPE string,
      dobj TYPE string,
    END OF tab_type.

  DATA: line TYPE tab_type,
        itab TYPE STANDARD TABLE OF tab_type,
        id   TYPE c LENGTH 10 VALUE 'ROMRETRO'.

  line-para = 'P1'.
  line-dobj = 'P_DATA_REM'.
  APPEND line TO itab.

  EXPORT (itab) TO MEMORY ID 'ROMRETRO'.

ENDFORM.                    " MEMORIZAR_DT_MOVIMENTO_BADI

FORM estorno_custo.

  DATA: lit_bdcdata TYPE TABLE OF bdcdata.

  DATA: lit_vfkp_estornar TYPE TABLE OF vfkp,
        lwa_zsdt0001      TYPE zsdt0001.

  DATA: lva_data(10).

  CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4) INTO lva_data.

  IF so_tknum[] IS INITIAL.
    MESSAGE 'Nenhum documento de transporte informado!' TYPE 'S'.
    RETURN.
  ENDIF.

  CLEAR: lit_vfkp_estornar[].

  SELECT *
    FROM vfkp INTO TABLE lit_vfkp_estornar
   WHERE rebel IN so_tknum.

  SORT lit_vfkp_estornar BY fknum.
  DELETE ADJACENT DUPLICATES FROM lit_vfkp_estornar COMPARING fknum.

  IF lit_vfkp_estornar[] IS INITIAL.
    MESSAGE 'Nenhum Documento de custo encontrado!' TYPE 'S'.
    RETURN.
  ENDIF.

  DATA(lva_docs_estornados) = 0.
  DATA(lva_docs_erro)       = 0.

  "********************************************************************************************************
  " SÒ PREVISTOS VI de FRETE CPT - Copiado Form Estorno_CUSTO ZLES0136
  "********************************************************************************************************

  LOOP AT lit_vfkp_estornar INTO DATA(lwa_vfkp_estornar).

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = | Processando Estorno Documento Custo { lwa_vfkp_estornar-fknum }... |.

    CLEAR: lwa_zsdt0001.

    SELECT SINGLE *
      FROM vttp INTO @DATA(lwa_vttp)
     WHERE tknum EQ @lwa_vfkp_estornar-rebel.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM vttk INTO @DATA(lwa_vttk)
     WHERE tknum EQ @lwa_vttp-tknum.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM zsdt0001 INTO lwa_zsdt0001
     WHERE doc_rem EQ lwa_vttp-vbeln.

    CLEAR: lit_bdcdata[].

    IF lwa_vttk-shtyp = 'Z001'.
      PERFORM f_bdc_data TABLES lit_bdcdata
                         USING:
          'SAPMV54A'  '0020'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_CURSOR'       'VFKK-FKNUM',
          ''          ''      ''   'BDC_OKCODE'       '=UEBP',
          ''          ''      ''   'VFKK-FKNUM'       lwa_vfkp_estornar-fknum, "fknum

          'SAPMV54A'  '0030'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(02)',
          ''          ''      ''   'BDC_OKCODE'       '=PLOE',
          ''          ''      ''   'VIM_MARKED(02)'   'X',

          'SAPLSPO1'  '0100'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'       '=YES',

          'SAPMV54A'  '0030'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(01)',
          ''          ''      ''   'BDC_OKCODE'       '=PDET',

          'SAPMV54A'  '0040'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'       '=PABR',

          'SAPMV54A'  '0040'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'       '=KLAC',
          ''          ''      ''  'VFKPD-SLSTOR'      'X',

          'SAPMV54A'  '0040'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'       '/00',
          ''          ''      ''   'VFKPD-STDAT'      lva_data,

          'SAPMV54A'  '0040'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'       '=SICH'.

    ELSE.
      PERFORM f_bdc_data TABLES lit_bdcdata
                          USING:
              'SAPMV54A'  '0020'  'X'  ''                 ' ',
              ''          ''      ''   'BDC_CURSOR'       'VFKK-FKNUM',
              ''          ''      ''   'BDC_OKCODE'       '=UEBP',
              ''          ''      ''   'VFKK-FKNUM'       lwa_vfkp_estornar-fknum, "fknum

              'SAPMV54A'  '0030'  'X'  ''                 ' ',
              ''          ''      ''   'BDC_CURSOR'       'VFKP-FKPOS(01)',
              ''          ''      ''   'BDC_OKCODE'       '=PDET',

              'SAPMV54A'  '0040'  'X'  ''                 ' ',
              ''          ''      ''   'BDC_OKCODE'       '=PABR',

              'SAPMV54A'  '0040'  'X'  ''                 ' ',
              ''          ''      ''   'BDC_OKCODE'       '=KLAC',
              ''          ''      ''  'VFKPD-SLSTOR'      'X',

              'SAPMV54A'  '0040'  'X'  ''                 ' ',
              ''          ''      ''   'BDC_OKCODE'       '/00',
              ''          ''      ''   'VFKPD-STDAT'      lva_data,

              'SAPMV54A'  '0040'  'X'  ''                 ' ',
              ''          ''      ''   'BDC_OKCODE'       '=SICH'.

    ENDIF.

    DATA(lva_erro) = abap_false.
    PERFORM f_call_transaction TABLES lit_bdcdata
                                USING 'VI02'
                             CHANGING lva_erro.

    IF lva_erro IS INITIAL.

      COMMIT WORK.
      WAIT UP TO 5 SECONDS.

      "Eliminar o documento de custo
      REFRESH lit_bdcdata.
      PERFORM f_bdc_data TABLES lit_bdcdata
                         USING:
             'SAPMV54A'  '0020'  'X'  ''                 ' ',
             ''          ''      ''   'BDC_CURSOR'       'VFKK-FKNUM',
             ''          ''      ''   'BDC_OKCODE'       '=UEBP',
             ''          ''      ''   'VFKK-FKNUM'       lwa_vfkp_estornar-fknum,

             'SAPMV54A'  '0030'  'X'  ''                 ' ',
             ''          ''      ''   'BDC_OKCODE'       '/ELOES'.

      CLEAR lva_erro.
      PERFORM f_call_transaction TABLES lit_bdcdata
                                  USING 'VI02'
                               CHANGING lva_erro.

      IF lva_erro IS INITIAL.
        COMMIT WORK.

        ADD 1 TO lva_docs_estornados.

        IF lwa_zsdt0001-ch_referencia IS NOT INITIAL.
          UPDATE zsdt0001 SET st_proc = '04'
                              fknum   = space
           WHERE ch_referencia EQ lwa_zsdt0001-ch_referencia.

          COMMIT WORK.
        ENDIF.

      ELSE.
        ADD 1 TO lva_docs_erro.
        CONTINUE.
      ENDIF.
    ELSE.
      ADD 1 TO lva_docs_erro.
    ENDIF.
  ENDLOOP.

  MESSAGE |Documentos Estornados: { lva_docs_estornados }! Documentos com erro no estorno: { lva_docs_erro } | TYPE 'I'.

ENDFORM.

FORM f_bdc_data TABLES t_bdcdata STRUCTURE bdcdata
                 USING p_program p_dynpro p_start p_fnam p_fval.

  DATA: lwa_bdcdata TYPE bdcdata.

  CLEAR lwa_bdcdata.
  lwa_bdcdata-program   = p_program.
  lwa_bdcdata-dynpro    = p_dynpro.
  lwa_bdcdata-dynbegin  = p_start.
  lwa_bdcdata-fnam      = p_fnam.
  lwa_bdcdata-fval      = p_fval.

  APPEND lwa_bdcdata TO t_bdcdata.

ENDFORM.


FORM f_call_transaction TABLES t_bdcdata STRUCTURE bdcdata
                         USING p_trans
                      CHANGING p_erro.

  DATA: BEGIN OF lit_msg OCCURS 0.
          INCLUDE STRUCTURE bdcmsgcoll.
  DATA: END OF lit_msg.

  CONSTANTS: c_msgid LIKE lit_msg-msgid VALUE 'F5',
             c_msgnr LIKE lit_msg-msgnr VALUE '312',
             c_msgne LIKE lit_msg-msgnr VALUE '539'.

  CLEAR: p_erro.

  DATA(lva_mode) = 'E'.

  CALL TRANSACTION p_trans USING t_bdcdata
              MODE lva_mode
     MESSAGES INTO lit_msg.

  READ TABLE lit_msg WITH KEY msgtyp = 'E'.
  IF sy-subrc = 0.
    p_erro = 'X'.
  ENDIF.

ENDFORM.                    "ZF_CALL_TRANSACTION
