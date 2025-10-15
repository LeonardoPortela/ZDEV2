class ZCL_GERAR_LOTE definition
  public
  final
  create public .

*"* public components of class ZCL_GERAR_LOTE
*"* do not include other source files here!!!
public section.

  class-methods CREATE_LOTE
    importing
      !I_BUKRS type BUKRS
      !I_DESCR_LOTE type ZDESCR_LOTE
      !I_DEP_RESP type CHAR2
      !I_USER_RESP type SY-UNAME
      !I_STATUS_LOTE type CHAR01 optional
    exporting
      value(E_NUM_LOTE) type ZLOTE_NUM .
  class-methods MODIFY_LOTE
    importing
      !I_NUM_LOTE type ZLOTE_NUM
      !I_BUKRS type BUKRS
      !I_DESCR_LOTE type ZDESCR_LOTE
      !I_DEP_RESP type CHAR2
      !I_USER_RESP type SY-UNAME .
  class-methods CONTABILIZAR_LOTE
    importing
      !I_ZGLT036_FLG type ZDE_ZGLT036_FLG_T optional
      !I_ARREDONDA type CHAR1 optional
    exporting
      !E_NUM_DOC type NUM10
    changing
      !I_ZGLT036 type ANY TABLE
      !I_ZGLT035 type ZGLT035 .
protected section.
*"* protected components of class ZCL_GERAR_LOTE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_GERAR_LOTE
*"* do not include other source files here!!!

  data AT_BUKRS type BUKRS .
ENDCLASS.



CLASS ZCL_GERAR_LOTE IMPLEMENTATION.


METHOD contabilizar_lote.
*----------------------------------------------------------
*  PARÂMETROS:
*     VALUE(I_ZGLT035) TYPE ZGLT035
*     VALUE(I_ZGLT036) TYPE TABLE ZGLT036

*  INFORMAÇÕES:
*  No parâmetro I_ZGLT035 é necessário passar as mesmas informações que são
*  preenchidas no cabeçalho da transação ZGL016.
*
*  No parâmetro I_ZGLT036 é necessário passar as informações dos itens.
*
*  Este método não substitui o uso da transação ZGL016. Apenas agiliza a
*  criação do DOC_NUM, para uso em outros programas.
*
*  AUTOR:
*  Enio Jesus - 24.08.2015
*---------------------------------------------------------
  DATA: gt_zglt036       TYPE TABLE OF zglt036,
        wl_zglt036       TYPE zglt036,
        wa_t001          TYPE t001,
        wa_moedas        TYPE x001,
        i_data           TYPE gdatu_inv,
        e_ukurs  	       TYPE ukurs_curr,
        wa_zglt036_flg   TYPE zde_zglt036_flg,
        wa_moeda_gp_hist TYPE zglt031-moeda_gp_hist.

  DATA: fg_doc(1),
        vlr_toler   TYPE zglt036-vlr_moeda_int,
        vlr_tot_int TYPE zglt036-vlr_moeda_int,
        vlr_tot_doc TYPE zglt036-vlr_moeda_doc,
        vlr_tot_gru TYPE zglt036-vlr_moeda_grupo,
        vlr_tot_for TYPE zglt036-vlr_moeda_forte.

  DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.
*---------------------------------------------------------*
*  GERA O NÚMERO DO DOCUMENTO                             *
*---------------------------------------------------------*
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = '01'
      object      = 'ZDOC_LCTO'
    IMPORTING
      number      = e_num_doc.

  i_zglt035-doc_lcto  = e_num_doc.


  SELECT SINGLE bukrs waers INTO (wa_t001-bukrs,wa_t001-waers)
    FROM t001
   WHERE bukrs EQ i_zglt035-bukrs.

  CALL FUNCTION 'FI_CURRENCY_INFORMATION'
    EXPORTING
      i_bukrs = i_zglt035-bukrs
    IMPORTING
      e_x001  = wa_moedas.

  IF i_zglt035-moeda_interna IS INITIAL.
    i_zglt035-moeda_interna = wa_t001-waers.
  ENDIF.

  IF i_zglt035-moeda_forte IS INITIAL.
    i_zglt035-moeda_forte = wa_moedas-hwae2.
  ENDIF.

  IF i_zglt035-moeda_grupo IS INITIAL.
    i_zglt035-moeda_grupo = wa_moedas-hwae3.
  ENDIF.

*---------------------------------------------------------*
*  INSERE OS REGISTROS DO CABEÇALHO NA TABELA             *
*---------------------------------------------------------*
  INSERT zglt035 FROM i_zglt035.
  COMMIT WORK.

*---------------------------------------------------------*
*  INSERE OS REGISTROS DOS ITENS NA TABELA                *
*---------------------------------------------------------*
  CREATE OBJECT obj_zcl_util_sd.

  IF ( sy-tcode = 'ZFI0081' ). "Pegar o 1º dia do próximo mês para busca de taxa

    DATA: _olddate TYPE sy-datum, _newdate TYPE sy-datum.

    _olddate = |{ i_zglt035-dt_lcto+0(4) }{ i_zglt035-dt_lcto+4(2) }01|.

    CALL FUNCTION 'RE_ADD_MONTH_TO_DATE'
      EXPORTING
        months  = 1
        olddate = _olddate
      IMPORTING
        newdate = _newdate.

    i_data = _newdate.

  ELSE.
    " realizar consulta na tabela ZGLT031 para verificar se o tipo de lançamento possui
    " um flag no campo MOEDA_GP_HIST, caso este estiver marcado, utilizar a data do documento
    " ao invés da data de lançamento - RSF
    CLEAR wa_moeda_gp_hist.

    SELECT SINGLE moeda_gp_hist
      INTO wa_moeda_gp_hist
      FROM zglt031
     WHERE tp_lcto EQ i_zglt035-tp_lcto.

    IF wa_moeda_gp_hist EQ 'X'.
      i_data = i_zglt035-bldat.
    ELSE.
      i_data = i_zglt035-dt_lcto.
    ENDIF.


  ENDIF.

  obj_zcl_util_sd->set_data(  EXPORTING i_data = i_data ).
  obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
  obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = i_zglt035-moeda_doc ).

  LOOP AT i_zglt036 INTO wl_zglt036.

    READ TABLE i_zglt036_flg INTO wa_zglt036_flg WITH KEY doc_lcto = wl_zglt036-doc_lcto
                                                          seqitem  = wl_zglt036-seqitem
                                                          seqsub   = wl_zglt036-seqsub.
    IF sy-subrc IS NOT INITIAL.
      CLEAR: wa_zglt036_flg.
    ENDIF.

    wl_zglt036-doc_lcto = e_num_doc.

    IF wl_zglt036-vlr_moeda_int EQ 0 AND wa_zglt036_flg-fl_cv_moeda_int IS INITIAL.
      IF i_zglt035-moeda_interna EQ i_zglt035-moeda_doc.
        wl_zglt036-vlr_moeda_int = wl_zglt036-vlr_moeda_doc.

      ELSE.
        obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = i_zglt035-moeda_interna ).
        obj_zcl_util_sd->taxa_cambio(  RECEIVING e_ukurs = e_ukurs ).
        IF e_ukurs LT 0.
*          IF I_ZGLT035-BUKRS EQ '0201' AND
*           ( I_ZGLT035-MOEDA_DOC EQ 'USD' AND I_ZGLT035-MOEDA_INTERNA EQ 'EUR' ).
*            WL_ZGLT036-VLR_MOEDA_INT = WL_ZGLT036-VLR_MOEDA_DOC * ABS( E_UKURS ).
*          ELSE.
          wl_zglt036-vlr_moeda_int = wl_zglt036-vlr_moeda_doc / abs( e_ukurs ).
*          ENDIF.
        ELSE.
*          IF I_ZGLT035-BUKRS EQ '0201' AND
*           ( I_ZGLT035-MOEDA_DOC EQ 'EUR' AND I_ZGLT035-MOEDA_INTERNA EQ 'USD' ).
*            WL_ZGLT036-VLR_MOEDA_INT = WL_ZGLT036-VLR_MOEDA_DOC / ABS( E_UKURS ).
*          ELSE.
          wl_zglt036-vlr_moeda_int = wl_zglt036-vlr_moeda_doc * abs( e_ukurs ).
*          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*    WL_ZGLT036-VLR_MOEDA_FORTE = '0.00'.

*    IF WL_ZGLT036-VLR_MOEDA_FORTE EQ 0 AND WA_ZGLT036_FLG-FL_CV_MOEDA_FOR IS INITIAL.
*      IF I_ZGLT035-MOEDA_FORTE EQ I_ZGLT035-MOEDA_DOC.
*        WL_ZGLT036-VLR_MOEDA_FORTE = WL_ZGLT036-VLR_MOEDA_DOC.
*      ELSE.
*        OBJ_ZCL_UTIL_SD->SET_TCURR( EXPORTING I_TCURR = I_ZGLT035-MOEDA_FORTE ).
*        OBJ_ZCL_UTIL_SD->TAXA_CAMBIO( RECEIVING E_UKURS = E_UKURS ).
*        IF E_UKURS LT 0.
**          IF I_ZGLT035-BUKRS EQ '0201' AND
**           ( I_ZGLT035-MOEDA_DOC EQ 'USD' AND I_ZGLT035-MOEDA_FORTE EQ 'EUR' ).
**            WL_ZGLT036-VLR_MOEDA_FORTE = WL_ZGLT036-VLR_MOEDA_DOC * ABS( E_UKURS ).
**          ELSE.
*          WL_ZGLT036-VLR_MOEDA_FORTE = WL_ZGLT036-VLR_MOEDA_DOC / ABS( E_UKURS ).
**          ENDIF.
*        ELSE.
**          IF I_ZGLT035-BUKRS EQ '0201' AND
**           ( I_ZGLT035-MOEDA_DOC EQ 'EUR' AND I_ZGLT035-MOEDA_FORTE EQ 'USD' ).
**            WL_ZGLT036-VLR_MOEDA_FORTE = WL_ZGLT036-VLR_MOEDA_DOC / ABS( E_UKURS ).
**          ELSE.
*          WL_ZGLT036-VLR_MOEDA_FORTE = WL_ZGLT036-VLR_MOEDA_DOC * ABS( E_UKURS ).
**          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDIF.

    IF wl_zglt036-vlr_moeda_grupo EQ 0 AND wa_zglt036_flg-fl_cv_moeda_gru IS INITIAL.
      IF i_zglt035-moeda_grupo EQ i_zglt035-moeda_doc.
        wl_zglt036-vlr_moeda_grupo = wl_zglt036-vlr_moeda_doc.
      ELSE.
        obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = i_zglt035-moeda_grupo ).
        obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = e_ukurs ).
        IF e_ukurs LT 0.
*          IF I_ZGLT035-BUKRS EQ '0201' AND
*           ( I_ZGLT035-MOEDA_DOC EQ 'USD' AND I_ZGLT035-MOEDA_GRUPO EQ 'EUR' ).
*            WL_ZGLT036-VLR_MOEDA_GRUPO = WL_ZGLT036-VLR_MOEDA_DOC * ABS( E_UKURS ).
*          ELSE.
          wl_zglt036-vlr_moeda_grupo = wl_zglt036-vlr_moeda_doc / abs( e_ukurs ).
*          ENDIF.
        ELSE.
*          IF I_ZGLT035-BUKRS EQ '0201' AND
*           ( I_ZGLT035-MOEDA_DOC EQ 'EUR' AND I_ZGLT035-MOEDA_GRUPO EQ 'USD' ).
*            WL_ZGLT036-VLR_MOEDA_GRUPO = WL_ZGLT036-VLR_MOEDA_DOC / ABS( E_UKURS ).
*          ELSE.
          wl_zglt036-vlr_moeda_grupo = wl_zglt036-vlr_moeda_doc * abs( e_ukurs ).
*          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    APPEND wl_zglt036 TO gt_zglt036.
  ENDLOOP.

  IF i_arredonda EQ abap_true.

    LOOP AT gt_zglt036 INTO DATA(wa36).

      SELECT SINGLE shkzg FROM tbsl
        INTO @DATA(shkzg)
        WHERE bschl EQ @wa36-bschl.

      IF shkzg NE 'S'.
        IF wa36-vlr_moeda_int   >= 0.
          wa36-vlr_moeda_int    = wa36-vlr_moeda_int   * -1.
        ENDIF.
        IF wa36-vlr_moeda_forte >= 0.
          wa36-vlr_moeda_forte  = wa36-vlr_moeda_forte * -1.
        ENDIF.
        IF wa36-vlr_moeda_grupo >= 0.
          wa36-vlr_moeda_grupo  = wa36-vlr_moeda_grupo * -1.
        ENDIF.
        IF wa36-vlr_moeda_doc >= 0.
          wa36-vlr_moeda_doc    = wa36-vlr_moeda_doc   * -1.
        ENDIF.
      ENDIF.

      ADD wa36-vlr_moeda_doc   TO vlr_tot_doc.
      ADD wa36-vlr_moeda_grupo TO vlr_tot_gru.
      ADD wa36-vlr_moeda_int   TO vlr_tot_int.
      ADD wa36-vlr_moeda_forte TO vlr_tot_for.

    ENDLOOP.

    vlr_toler = 2 / 100.

    LOOP AT gt_zglt036 ASSIGNING FIELD-SYMBOL(<wa36>).

      SELECT SINGLE shkzg FROM tbsl
        INTO shkzg
        WHERE bschl EQ <wa36>-bschl.

      "LP BUG ARREDONDAMENTO  #102071 ZGL071
      IF  sy-tcode EQ 'ZGL071' AND sy-tcode EQ 'ZFIT0189'.
        IF vlr_tot_doc GT 0 AND vlr_tot_doc LE vlr_toler .
          IF <wa36>-bschl EQ '50' AND shkzg EQ 'H'. " OR <wa36>-bschl EQ '40'.  "Total do DOcumento Menor que tolerancia
            <wa36>-vlr_moeda_doc = <wa36>-vlr_moeda_doc + abs( vlr_tot_doc ).
            CLEAR vlr_tot_doc.
          ENDIF.
          IF <wa36>-bschl EQ '40' AND shkzg EQ 'S'. "OR <wa36>-bschl EQ '40'.  "Total do DOcumento Menor que tolerancia
            <wa36>-vlr_moeda_doc = <wa36>-vlr_moeda_doc - abs( vlr_tot_doc ).
            CLEAR vlr_tot_doc.
          ENDIF.
        ENDIF.
        IF vlr_tot_doc LT 0  AND vlr_tot_doc GE ( vlr_toler * ( -1 ) ) .
          IF <wa36>-bschl EQ '50' AND shkzg EQ 'H'. " OR <wa36>-bschl EQ '40'."Total do DOcumento maior que tolerancia
            "<wa36>-vlr_moeda_doc = <wa36>-vlr_moeda_doc + abs( vlr_tot_doc ).
            <wa36>-vlr_moeda_doc = <wa36>-vlr_moeda_doc - abs( vlr_tot_doc ).
            CLEAR vlr_tot_doc.
          ENDIF.
          IF <wa36>-bschl EQ '40' AND shkzg EQ 'S'." OR <wa36>-bschl EQ '40'."Total do DOcumento maior que tolerancia
            "<wa36>-vlr_moeda_doc = <wa36>-vlr_moeda_doc + abs( vlr_tot_doc ).
            <wa36>-vlr_moeda_doc = <wa36>-vlr_moeda_doc + abs( vlr_tot_doc ).
            CLEAR vlr_tot_doc.
          ENDIF.
        ENDIF.
        "FORTE VLR_TOT_FOR

        IF vlr_tot_for GT 0 AND vlr_tot_for LE vlr_toler.
          IF  <wa36>-bschl EQ '50' AND shkzg EQ 'H'.." OR <wa36>-bschl EQ '40'.  "Moeda Forte Maior  que tolerancia
            <wa36>-vlr_moeda_forte = <wa36>-vlr_moeda_forte + abs( vlr_tot_for ).
            CLEAR vlr_tot_for.
          ENDIF.
          IF  <wa36>-bschl EQ '40' AND shkzg EQ 'S'." OR <wa36>-bschl EQ '40'.  "Moeda Forte Maior  que tolerancia - RJF
            <wa36>-vlr_moeda_forte = <wa36>-vlr_moeda_forte - abs( vlr_tot_for ).
            CLEAR vlr_tot_for.
          ENDIF.
        ENDIF.
        IF vlr_tot_for LT 0  AND vlr_tot_for GE ( vlr_toler * ( -1 ) ).
          IF <wa36>-bschl EQ '50' AND shkzg EQ 'H'." OR <wa36>-bschl EQ '40'."Moeda Forte menor que tolerancia
            "  <wa36>-vlr_moeda_forte = <wa36>-vlr_moeda_forte + abs( vlr_tot_for ).
            <wa36>-vlr_moeda_forte = <wa36>-vlr_moeda_forte - abs( vlr_tot_for ).
            CLEAR vlr_tot_for.
          ENDIF.
          IF <wa36>-bschl EQ '40' AND shkzg EQ 'S'." OR <wa36>-bschl EQ '40'."Moeda Forte menor que tolerancia - RJF
            "  <wa36>-vlr_moeda_forte = <wa36>-vlr_moeda_forte + abs( vlr_tot_for ).
            <wa36>-vlr_moeda_forte = <wa36>-vlr_moeda_forte + abs( vlr_tot_for ).
            CLEAR vlr_tot_for.
          ENDIF.
        ENDIF.

        " "INTERNO
        IF vlr_tot_int GT 0 AND vlr_tot_int LE vlr_toler.
          IF  <wa36>-bschl EQ '50' AND shkzg EQ 'H'." OR <wa36>-bschl EQ '40'.  "Moeda INTERNA  Maior  que tolerancia
            <wa36>-vlr_moeda_int = <wa36>-vlr_moeda_int + abs( vlr_tot_int ).
            CLEAR vlr_tot_int.
          ENDIF.
          IF  <wa36>-bschl EQ '40' AND shkzg EQ 'S'. " OR <wa36>-bschl EQ '40'.  "Moeda INTERNA  Maior  que tolerancia
            <wa36>-vlr_moeda_int = <wa36>-vlr_moeda_int - abs( vlr_tot_int ).
            CLEAR vlr_tot_int.
          ENDIF.
        ENDIF.
        IF vlr_tot_int LT 0  AND vlr_tot_int GE ( vlr_toler * ( -1 ) ) .
          IF <wa36>-bschl EQ '50' AND shkzg EQ 'H'. "OR <wa36>-bschl EQ '40'."Moeda INTERNA menor que tolerancia
            "  <wa36>-vlr_moeda_int = <wa36>-vlr_moeda_int + abs( vlr_tot_int ).
            <wa36>-vlr_moeda_int = <wa36>-vlr_moeda_int - abs( vlr_tot_int ).
            CLEAR vlr_tot_int.
          ENDIF.
          IF <wa36>-bschl EQ '40' AND shkzg EQ 'S'. "OR <wa36>-bschl EQ '40'."Moeda INTERNA menor que tolerancia
            "  <wa36>-vlr_moeda_int = <wa36>-vlr_moeda_int + abs( vlr_tot_int ).
            <wa36>-vlr_moeda_int = <wa36>-vlr_moeda_int + abs( vlr_tot_int ).
            CLEAR vlr_tot_int.
          ENDIF.
        ENDIF.

      ENDIF.

      "<<< "LP BUG ARREDONDAMENTO  #102071 ZGL071


      IF vlr_tot_doc = 0.

        "INTERNO
        IF abs( vlr_tot_int ) GT 0
       AND abs( vlr_tot_int ) LE vlr_toler. "Arredondar

          IF vlr_tot_int GT 0.
            IF shkzg = 'S'. " EQ D
              SUBTRACT vlr_tot_int FROM <wa36>-vlr_moeda_int.
              EXIT.
            ENDIF.
          ELSE.
            IF shkzg NE 'S'. " EQ C
              SUBTRACT vlr_tot_int FROM <wa36>-vlr_moeda_int.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.


        "FORTE
        IF abs( vlr_tot_for ) GT 0
       AND abs( vlr_tot_for ) LE vlr_toler. "Arredondar
          IF vlr_tot_for GT 0.
            IF shkzg = 'S'. " EQ D
              SUBTRACT vlr_tot_for FROM <wa36>-vlr_moeda_forte.
              EXIT.
            ENDIF.
          ELSE.
            IF shkzg NE 'S'. " EQ C
              SUBTRACT vlr_tot_for FROM <wa36>-vlr_moeda_forte.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.

        "GRUPO
        IF abs( vlr_tot_gru ) GT 0
       AND abs( vlr_tot_gru ) LE vlr_toler. "Arredondar
          IF vlr_tot_gru GT 0.
            IF shkzg = 'S'. " EQ D
              SUBTRACT vlr_tot_gru FROM <wa36>-vlr_moeda_grupo.
              EXIT.
            ENDIF.
          ELSE.
            IF shkzg NE 'S'. " EQ C
              SUBTRACT vlr_tot_gru FROM <wa36>-vlr_moeda_grupo.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDIF.

  INSERT zglt036 FROM TABLE gt_zglt036.
  COMMIT WORK.

ENDMETHOD.


METHOD CREATE_LOTE.
  DATA: WL_ZGLT034 TYPE ZGLT034,
        V_NUMBER   TYPE I.

  CLEAR WL_ZGLT034.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR = '01'
      OBJECT      = 'ZGL_LOTE'
    IMPORTING
      NUMBER      = V_NUMBER.

  MOVE:
  V_NUMBER      TO WL_ZGLT034-LOTE,
  V_NUMBER      TO E_NUM_LOTE,
  I_DESCR_LOTE  TO WL_ZGLT034-DESCR_LOTE,
  I_BUKRS       TO WL_ZGLT034-BUKRS,
  I_DEP_RESP    TO WL_ZGLT034-DEP_RESP,
  I_USER_RESP   TO WL_ZGLT034-USNAM,
  SY-DATUM      TO WL_ZGLT034-DATA_ATUAL,
  SY-UZEIT      TO WL_ZGLT034-HORA_ATUAL,
  SY-UNAME      TO WL_ZGLT034-USUARIO,
  I_STATUS_LOTE TO WL_ZGLT034-STATUS_LOTE.
  WL_ZGLT034-tcode = sy-tcode.
  INSERT ZGLT034 FROM WL_ZGLT034.
  COMMIT WORK.

ENDMETHOD.


METHOD MODIFY_LOTE.
  DATA: WL_ZGLT034 TYPE ZGLT034.
  CLEAR WL_ZGLT034.

  MOVE:
  I_NUM_LOTE   TO WL_ZGLT034-LOTE,
  I_DESCR_LOTE TO WL_ZGLT034-DESCR_LOTE,
  I_BUKRS      TO WL_ZGLT034-BUKRS,
  I_DEP_RESP   TO WL_ZGLT034-DEP_RESP,
  I_USER_RESP  TO WL_ZGLT034-USNAM,
  SY-UNAME     TO WL_ZGLT034-USUARIO.

  WL_ZGLT034-TCODE = SY-TCODE.

  MODIFY ZGLT034 FROM WL_ZGLT034.
  COMMIT WORK.


ENDMETHOD.
ENDCLASS.
