*&---------------------------------------------------------------------*
*& Report ZFIR0100_JOB
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir0100_processa.

TABLES: acdoca,zib_contabil,sscrfields.

DATA: lv_message TYPE string.

TYPES: BEGIN OF ty_saida,
         obj_key       TYPE string,
         xblnr         TYPE zib_contabil-xblnr,
         belnr         TYPE belnr_d,
         seqitem(3)    TYPE c, "zib_contabil-seqitem,
         bschl         TYPE zib_contabil-bschl,
         gsber         TYPE zib_contabil-gsber,
         bukrs         TYPE zib_contabil-bukrs,
         bldat         TYPE zib_contabil-bldat,
         budat         TYPE zib_contabil-budat,
         gjahr         TYPE zib_contabil-gjahr,
         monat         TYPE zib_contabil-monat,
         blart         TYPE zib_contabil-blart,
         hkont         TYPE zib_contabil-hkont,
         wrbtr         TYPE zib_contabil-wrbtr,
         waers         TYPE zib_contabil-waers,
         sgtxt         TYPE zib_contabil-sgtxt,
         kostl         TYPE zib_contabil-prctr,
         prctr         TYPE zib_contabil-prctr,
         waers_i       TYPE zib_contabil-waers_i,
         dmbtr         TYPE zib_contabil-dmbtr,
         waers_f       TYPE zib_contabil-waers_f,
         dmbe2         TYPE zib_contabil-dmbe2,
         rg_atualizado TYPE zib_contabil-rg_atualizado,
         rldnr(2)      TYPE c,
         status        TYPE string,
       END OF ty_saida.

TYPES: BEGIN OF ty_xblnr,
         xbnlr TYPE zib_contabil-xblnr,
       END OF ty_xblnr.

TYPES: BEGIN OF ty_selecao,
         rbukrs      TYPE acdoca-rbukrs,
         gjahr       TYPE acdoca-gjahr,
         belnr       TYPE acdoca-belnr,
         obj_key(20) TYPE c,
       END OF ty_selecao.
DATA: it_selecao TYPE STANDARD TABLE OF ty_selecao INITIAL SIZE 0.


DATA: it_acdoca TYPE STANDARD TABLE OF acdoca INITIAL SIZE 0.
DATA: it_zib_contabil TYPE STANDARD TABLE OF zib_contabil INITIAL SIZE 0.
DATA: it_zib_contabil_CHV TYPE STANDARD TABLE OF zib_contabil_chv INITIAL SIZE 0.
DATA: it_zib_contabil_ERR TYPE STANDARD TABLE OF zib_contabil_err INITIAL SIZE 0.
DATA: lt_zib_contabil TYPE zib_contabil.
DATA: lt_zib_contabil_CHV TYPE zib_contabil_chv.
DATA: lt_zib_contabil_ERR TYPE zib_contabil_err.
DATA lr_xblnr TYPE RANGE OF xblnr.
DATA: it_saida TYPE STANDARD TABLE OF ty_saida INITIAL SIZE 0.
DATA: it_save  TYPE STANDARD TABLE OF ty_saida INITIAL SIZE 0.
DATA: wa_zib_contabil TYPE zib_contabil.
DATA: wa_saida TYPE ty_saida.
DATA: wa_save  TYPE ty_saida.
DATA: it_xbelnr TYPE STANDARD TABLE OF ty_xblnr INITIAL SIZE 0 .
DATA: it_ZIB_xbelnr TYPE STANDARD TABLE OF ty_xblnr INITIAL SIZE 0 .
DATA: lr_BELNR TYPE RANGE OF zib_contabil-xblnr.
DATA: lr_xBELNR TYPE RANGE OF zib_contabil-xblnr.
DATA: UP_skb1 TYPE STANDARD TABLE OF skb1 WITH HEADER LINE.
DATA: DOWN_skb1 TYPE STANDARD TABLE OF skb1 WITH HEADER LINE.
DATA: DOWN2_skb1 TYPE STANDARD TABLE OF skb1 WITH HEADER LINE.
DATA: v_radio(3) TYPE c.
DATA: dtini   TYPE sy-datum.
DATA: dtfim   TYPE sy-datum.
DATA: hj      TYPE sy-datum.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-001.
  PARAMETERS:       p_emp  TYPE bukrs.
  PARAMETERS:       p_ano  TYPE gjahr.
  PARAMETERS:       p_mes(2)  TYPE c. .
SELECTION-SCREEN END OF BLOCK a.

**********************************************************************
START-OF-SELECTION.

  PERFORM grava_check_contas.

  PERFORM define_data.

  PERFORM documentos_processar.

  APPEND LINES OF it_saida TO it_save.

  IF it_save IS NOT INITIAL.
    PERFORM gera_contabilizacao.
  ELSE.
    CLEAR: lv_message,it_acdoca.
    lv_message =  |Não Existem dados para esta Empresa/Período!|.
    MESSAGE lv_message TYPE 'I'.
    STOP.

  ENDIF.

END-OF-SELECTION.

FORM grava_check_contas .

  TYPES: BEGIN OF contas,
           mandt TYPE skb1-mandt,
           racct TYPE skb1-saknr,
         END OF contas.

  DATA: it_contas TYPE STANDARD TABLE OF contas INITIAL SIZE 0.

  SELECT DISTINCT a~mandt, a~saknr
  INTO TABLE @it_contas
  FROM skb1 AS a
  LEFT JOIN tvarvc AS b ON a~bukrs = b~low AND b~name = 'ZFIR0100_LEDGER50'
  WHERE a~mitkz = 'A'
  AND a~saknr NOT IN ( SELECT DISTINCT racct FROM zfit0100_cont ).

  IF it_contas IS NOT INITIAL.
    LOOP AT it_contas ASSIGNING FIELD-SYMBOL(<conta_save>).
      MODIFY zfit0100_cont FROM TABLE it_contas.
    ENDLOOP.
    COMMIT WORK.
  ENDIF.

  CLEAR: it_contas.
ENDFORM.

FORM documentos_processar .

  TYPES: BEGIN OF ty_validacao_obj_key,
           obj_key   TYPE zib_contabil_chv-obj_key,
           existe(3) TYPE c,
         END OF ty_validacao_obj_key.

  DATA: it_obj_key TYPE STANDARD TABLE OF ty_validacao_obj_key WITH HEADER LINE. "Sim Encontrado nas ZIB's
  DATA aux_belnr TYPE belnr_d.
  DATA: it_filtro_zibchv TYPE STANDARD TABLE OF ty_selecao INITIAL SIZE 0.


  CALL FUNCTION 'GET_DATA_ZFIR0100'
    EXPORTING
      i_bukrs    = p_emp
      i_gjahr    = p_ano
      i_dtini    = dtini
      i_dtfim    = dtfim
    IMPORTING
      e_zfir0100 = it_selecao.


  IF it_selecao IS NOT INITIAL.

    LOOP AT it_selecao ASSIGNING FIELD-SYMBOL(<get_validacao_obj_key>) GROUP BY <get_validacao_obj_key>-obj_key.

      DATA(obj_key_tmp) = <get_validacao_obj_key>-obj_key.

      SELECT SINGLE a~obj_key,chv~obj_key AS obj_key_chv,err~obj_key AS obj_key_err
      FROM zib_contabil AS a
      LEFT JOIN zib_contabil_chv AS chv ON a~obj_key = chv~obj_key
      LEFT JOIN zib_contabil_ERR AS err ON a~obj_key = err~obj_key
      WHERE a~obj_key = @obj_key_tmp
      INTO @DATA(aux_obj_key).

      IF aux_obj_key IS NOT INITIAL.
        IF aux_obj_key-obj_key_chv IS NOT INITIAL.
          it_obj_key-obj_key = aux_obj_key-obj_key.
          it_obj_key-existe = 'CHV'.
        ELSE.
          IF aux_obj_key-obj_key_err IS NOT INITIAL.
            it_obj_key-obj_key = aux_obj_key-obj_key.
            it_obj_key-existe = 'ERR'.
          ENDIF.
        ENDIF.
      ELSE.
        it_obj_key-obj_key = <get_validacao_obj_key>-obj_key.
        it_obj_key-existe = 'NOT'.
      ENDIF.
      CLEAR: aux_obj_key.
      FREE: aux_obj_key.
      APPEND it_obj_key.
    ENDLOOP.

    LOOP AT it_obj_key ASSIGNING FIELD-SYMBOL(<del_processado>) WHERE existe = 'CHV' GROUP BY <del_processado>-obj_key.
      DELETE it_selecao WHERE obj_key = <del_processado>-obj_key.
    ENDLOOP.

    IF it_selecao IS NOT INITIAL.

      LOOP AT it_selecao ASSIGNING FIELD-SYMBOL(<devolve_zero_belnr>).
        UNPACK <devolve_zero_belnr>-belnr TO <devolve_zero_belnr>-belnr.
      ENDLOOP.

      SELECT *
      FROM acdoca AS a
      FOR ALL ENTRIES IN @it_selecao
      WHERE a~belnr = @it_selecao-belnr
      AND a~rbukrs = @it_selecao-rbukrs
      AND a~gjahr = @it_selecao-gjahr
      AND substring( a~belnr,1,1 ) IN ('0','1','2','3','4','5','6','7','8','9')
      INTO TABLE @it_acdoca.

      DELETE it_acdoca WHERE hsl = 0 AND ksl = 0.
      DELETE it_acdoca WHERE racct = '0000212200'.

      IF it_acdoca IS NOT INITIAL.

        LOOP AT it_acdoca ASSIGNING FIELD-SYMBOL(<remove_zero_belnr>).
          PACK <remove_zero_belnr>-belnr TO <remove_zero_belnr>-belnr.
          CONDENSE <remove_zero_belnr>-belnr NO-GAPS.
        ENDLOOP.

        DATA row TYPE i.

        SORT it_acdoca ASCENDING BY rbukrs gjahr belnr.

        LOOP AT it_acdoca ASSIGNING FIELD-SYMBOL(<acdoca>).
          CONDENSE <acdoca>-belnr NO-GAPS.
          UNPACK <acdoca>-belnr TO wa_saida-belnr.

          wa_saida-obj_key        = |LED{ <acdoca>-rbukrs }{ <acdoca>-belnr }|.

          SELECT SINGLE a~* FROM zib_contabil AS a
          WHERE a~obj_key = @wa_saida-obj_key
          INTO  @lt_zib_contabil.

          SELECT SINGLE a~* FROM zib_contabil_chv AS a
          WHERE a~obj_key = @wa_saida-obj_key
          INTO @lt_zib_contabil_chv.

          SELECT SINGLE a~* FROM zib_contabil_err AS a
          WHERE a~obj_key = @wa_saida-obj_key
          INTO @lt_zib_contabil_err.

          wa_saida-rg_atualizado  = 'N'.
          wa_saida-rldnr          = '50'.

          IF lt_zib_contabil_chv-obj_key = wa_saida-obj_key.
            wa_saida-xblnr  = lt_zib_contabil_chv-belnr.
          ELSE.
            CLEAR: wa_saida-xblnr.
          ENDIF.

          IF <acdoca>-bschl = '75'. "Valor Negativo
            wa_saida-bschl = '50'.
          ELSEIF <acdoca>-bschl = '70'. "Valor Negativo
            wa_saida-bschl = '40'.
          ELSE.
            wa_saida-bschl = <acdoca>-bschl.
          ENDIF.

          IF <acdoca>-tsl < 0.
            <acdoca>-tsl = <acdoca>-tsl * -1.
          ENDIF.

          IF <acdoca>-hsl < 0.
            <acdoca>-hsl = <acdoca>-hsl * -1.
          ENDIF.

          IF <acdoca>-ksl < 0.
            <acdoca>-ksl = <acdoca>-ksl * -1.
          ENDIF.

          IF <acdoca>-hsl = 0 OR <acdoca>-ksl = 0.
            wa_saida-waers_i        = 'X'.
          ELSE.
            wa_saida-waers_i        = <acdoca>-rhcur.
          ENDIF.

          wa_saida-gsber          = <acdoca>-rbusa.
          wa_saida-bukrs          = <acdoca>-rbukrs.
          wa_saida-bldat          = |{ <acdoca>-bldat+6(2) }.{ <acdoca>-bldat+4(2) }.{ <acdoca>-bldat+0(4) }|.
          wa_saida-budat          = |{ <acdoca>-budat+6(2) }.{ <acdoca>-budat+4(2) }.{ <acdoca>-budat+0(4) }|.
          wa_saida-gjahr          = <acdoca>-gjahr.
          wa_saida-monat          = <acdoca>-poper.
          wa_saida-blart          = <acdoca>-blart.
          CONDENSE <acdoca>-racct NO-GAPS.
          UNPACK <acdoca>-racct TO wa_saida-hkont.
          wa_saida-wrbtr          = <acdoca>-tsl.
          wa_saida-waers          = <acdoca>-rwcur.
          wa_saida-sgtxt          = <acdoca>-sgtxt.
          wa_saida-kostl          = <acdoca>-rcntr.
          wa_saida-prctr          = <acdoca>-prctr.
          wa_saida-dmbtr          = <acdoca>-hsl.
          wa_saida-waers_f        = <acdoca>-rkcur.
          wa_saida-dmbe2          = <acdoca>-ksl.

          APPEND wa_saida TO it_saida.

        ENDLOOP.
        "Cria Sequência
        LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<seq1>) GROUP BY ( bukrs = <seq1>-bukrs gjahr = <seq1>-gjahr belnr = <seq1>-belnr ).
          CLEAR: row.
          LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<seq2>) WHERE bukrs = <seq1>-bukrs AND gjahr = <seq1>-gjahr AND belnr = <seq1>-belnr .
            row = row + 1.
            UNPACK row TO <seq2>-seqitem.
          ENDLOOP.
        ENDLOOP.

      ELSE.

        PERFORM sem_dados.

      ENDIF.

    ELSE.

      PERFORM sem_dados.

    ENDIF.
  ELSE.

    PERFORM sem_dados.

  ENDIF.

ENDFORM.

FORM gera_contabilizacao.

  IF it_save IS NOT INITIAL.

    CLEAR: wa_saida,it_zib_contabil.

    LOOP AT it_save INTO wa_saida.

      PACK  wa_saida-seqitem TO wa_zib_contabil-seqitem.

      wa_zib_contabil-mandt         = sy-mandt.
      wa_zib_contabil-bldat         = wa_saida-bldat.
      wa_zib_contabil-bschl         = wa_saida-bschl.
      wa_zib_contabil-budat         = wa_saida-budat.
      wa_zib_contabil-bukrs         = wa_saida-bukrs.
      wa_zib_contabil-dmbe2         = wa_saida-dmbe2.
      wa_zib_contabil-dmbtr         = wa_saida-dmbtr.
      wa_zib_contabil-gjahr         = wa_saida-gjahr.
      wa_zib_contabil-gsber         = wa_saida-gsber.
      wa_zib_contabil-hkont         = wa_saida-hkont.
      wa_zib_contabil-kostl         = wa_saida-kostl.
      wa_zib_contabil-monat         = wa_saida-monat.
      wa_zib_contabil-obj_key       = wa_saida-obj_key.
      wa_zib_contabil-prctr         = wa_saida-prctr.
      wa_zib_contabil-rg_atualizado = wa_saida-rg_atualizado.
      wa_zib_contabil-rldnr         = wa_saida-rldnr.
      wa_zib_contabil-sgtxt         = wa_saida-sgtxt.
      wa_zib_contabil-waers         = wa_saida-waers.
      wa_zib_contabil-waers_f       = wa_saida-waers_f.
      wa_zib_contabil-waers_i       = wa_saida-waers_i.
      wa_zib_contabil-wrbtr         = wa_saida-wrbtr.
      wa_zib_contabil-blart         = wa_saida-blart.
      wa_zib_contabil-xblnr         = wa_saida-belnr.

      APPEND wa_zib_contabil TO it_zib_contabil.

    ENDLOOP.

    CLEAR: wa_zib_contabil,wa_save.

    DATA(it_zib_contabil_up) = it_zib_contabil.

    SORT it_zib_contabil_up BY bukrs hkont ASCENDING.

    DELETE ADJACENT DUPLICATES FROM it_zib_contabil_up COMPARING bukrs hkont.

    TYPES: BEGIN OF ty_zfit0100_SAVE,
             obj_key TYPE zib_contabil-obj_key,
             bukrs   TYPE zib_contabil-bukrs,
             hkont   TYPE zib_contabil-hkont,
           END OF ty_zfit0100_SAVE.


    DATA: zfit0100_SAVE TYPE STANDARD TABLE OF ty_zfit0100_SAVE WITH HEADER LINE.

    LOOP AT it_zib_contabil_up ASSIGNING FIELD-SYMBOL(<up>) GROUP BY ( bukrs = <up>-bukrs hkont = <up>-hkont ).
      UP_skb1-bukrs =  <up>-bukrs.
      UP_skb1-saknr =  <up>-hkont.
      APPEND UP_skb1.
    ENDLOOP.

    LOOP AT it_zib_contabil_up ASSIGNING FIELD-SYMBOL(<tmp>) GROUP BY ( bukrs = <tmp>-bukrs obj_key = <tmp>-obj_key hkont = <tmp>-hkont ).
      zfit0100_save-obj_key = <tmp>-obj_key.
      zfit0100_save-bukrs = <tmp>-bukrs.
      zfit0100_save-hkont = <tmp>-hkont.
      APPEND zfit0100_save.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM zfit0100_save COMPARING  bukrs obj_key hkont.
    DELETE ADJACENT DUPLICATES FROM UP_skb1 COMPARING bukrs saknr.


    LOOP AT zfit0100_save[] ASSIGNING FIELD-SYMBOL(<contas>) GROUP BY ( hkont = <contas>-hkont bukrs = <contas>-bukrs ).

      SELECT SINGLE racct FROM zfit0100_cont WHERE racct = @<contas>-hkont INTO @DATA(pode_alterar).

      UPDATE skb1 SET mitkz = @abap_false WHERE saknr = @<contas>-hkont AND bukrs = @<contas>-bukrs.
      COMMIT WORK.

    ENDLOOP.

    "CLEAR: it_contas_a.

    CLEAR: UP_skb1[],UP_skb1.

    "Grava ZibContabil
    MODIFY zib_contabil FROM TABLE it_zib_contabil.
    COMMIT WORK.

    DATA: IT_zfit0100_chv TYPE STANDARD TABLE OF zfit0100_chv WITH HEADER LINE.

    LOOP AT it_zib_contabil_up INTO DATA(wa_tmp) GROUP BY ( obj_key = wa_tmp-obj_key )."bukrs = wa_tmp-bukrs hkont = wa_tmp-hkont ).
      IT_zfit0100_chv-mandt = sy-mandt.
      IT_zfit0100_chv-obj_key = wa_tmp-obj_key.
      APPEND IT_zfit0100_chv.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM IT_zfit0100_chv COMPARING obj_key.

    MODIFY zfit0100_chv FROM TABLE IT_zfit0100_chv[].
    COMMIT WORK.

    CLEAR: it_zib_contabil,it_zib_contabil_up,zfit0100_save,IT_zfit0100_chv[].

  ENDIF.

ENDFORM.

FORM sem_dados.
  CLEAR: lv_message,it_acdoca.
  lv_message =  |Não Existem dados para esta Empresa/Período!|.
  MESSAGE lv_message TYPE 'I'.
  STOP.
ENDFORM.

FORM define_data.

  CLEAR: dtini,dtfim,hj.

  IF sy-batch IS INITIAL.
    DATA(periodo) = |{ p_ano }{ p_mes }|.
    dtini = |{ periodo }01|.
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = dtini
      IMPORTING
        last_day_of_month = dtfim
      EXCEPTIONS
        OTHERS            = 02.
  ENDIF.

ENDFORM.
