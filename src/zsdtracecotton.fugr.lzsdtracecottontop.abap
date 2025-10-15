FUNCTION-POOL zsdtracecotton             MESSAGE-ID sv.

DATA: it_0165  TYPE TABLE OF zsdt0165,
      lv_bukrs TYPE bukrs,
      lv_werks TYPE werks_d.

* INCLUDE LZSDTRACECOTTOND...                " Local class definition

CLASS zcl_tracecotton DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      getmaterial IMPORTING tipo          TYPE normt
                  RETURNING VALUE(return) TYPE matnr,

      "Projeto Reestruturação Algodao 2024
      "getfazenda IMPORTING fazenda       TYPE any
      "           RETURNING VALUE(return) TYPE werks_d,
      getempresa IMPORTING werks         TYPE werks_d
                 RETURNING VALUE(return) TYPE bukrs,

      "Projeto Reestruturação Algodao 2024
*      getalgodoeira IMPORTING "werks       TYPE WERKS_D    "bug 64768
*                              werks         TYPE any
*                              lote          TYPE  any " CHARG_D
*                              safra         TYPE char4
*                    RETURNING VALUE(return) TYPE string,

      get0143  IMPORTING contrato      TYPE bstkd
                         bukrs         TYPE bukrs
                         safra         TYPE zsdt0165-safra  "*-CS2023000189-05.04.2023-#108694-JT
               RETURNING VALUE(return) TYPE zsdt0143,
      getstatus IMPORTING status        TYPE any
                RETURNING VALUE(return) TYPE z_status_trace,
      setconvertqtd IMPORTING qtd           TYPE char10
                    RETURNING VALUE(return) TYPE gsgew,
      setconvertdat IMPORTING data          TYPE char50
                    RETURNING VALUE(return) TYPE sy-datum,
      getseq RETURNING VALUE(return) TYPE numc15,
      getseq_0178 RETURNING VALUE(return) TYPE numc15,
      set_erros IMPORTING input         TYPE zsdt0165
                RETURNING VALUE(return) TYPE char1,
      get_exit_atinn_input IMPORTING caracteristica TYPE string
                           RETURNING VALUE(return)  TYPE atinn,
      get_mchb IMPORTING matnr         TYPE matnr
                         werks         TYPE werks_d
                         lgort         TYPE lgort_d
                         charg         TYPE charg_d
               RETURNING VALUE(return) TYPE mchb,
      get_mch1 IMPORTING matnr         TYPE matnr
                         charg         TYPE charg_d
               RETURNING VALUE(return) TYPE cuobj_bm,
      get_ausp IMPORTING objek         TYPE objnum
                         atinn         TYPE atinn
               RETURNING VALUE(return) TYPE sy-subrc.

ENDCLASS.

CLASS zcl_tracecotton  IMPLEMENTATION.

  METHOD getmaterial.
    SELECT SINGLE matnr
      FROM mara
      INTO return
      WHERE normt EQ tipo
        AND mtart EQ 'ZFER'.
  ENDMETHOD.

*  METHOD getfazenda.
*    SELECT SINGLE werks
*      FROM zsdt0176
*      INTO return
*      WHERE empresa EQ fazenda.
*  ENDMETHOD.

  METHOD getempresa.
    SELECT SINGLE vkorg
      FROM zsdt_depara_cen
      INTO @DATA(lv_vkorg)
      WHERE centro_real = @werks.
    return = lv_vkorg.
  ENDMETHOD.

*  METHOD getalgodoeira.                                     " BUG64768
*
*    DATA: lv_verid TYPE zppt0002-verid.
*
*    CLEAR: lv_verid.
*    CLEAR:return.
*    SELECT SINGLE verid
*      FROM zppt0002
*      INTO lv_verid
*      WHERE werks EQ werks
*      AND   lgort EQ lote
*      AND   cd_safra EQ safra.
*
*    IF lv_verid IS NOT INITIAL.
*
*      SELECT SINGLE algodoeira
*        FROM zppt0004
*        INTO return
*        WHERE werks EQ werks
*        AND verid EQ lv_verid.
*
*      IF  return IS INITIAL.
*        return = |V { lv_verid }|. "Caso não encontrar descrição da Algodoeira na tabela zppt0004 retornar Verid para compor mensagem de erro
*      ENDIF.
*
*
*    ENDIF.
*
*
*  ENDMETHOD.

  METHOD get0143.
    SELECT SINGLE *
      FROM zsdt0143
      INTO return
      WHERE contrato  EQ contrato
        AND empresa   EQ bukrs
        AND safra     EQ safra  "*-CS2023000189-05.04.2023-#108694-JT-inicio
        AND cancelado <> abap_true.
  ENDMETHOD.

  METHOD getstatus.
    return = SWITCH #( status WHEN 'Aprovado'  THEN 'A'
                              WHEN 'Recusado'  THEN 'R'
                              WHEN 'Reprovado' THEN 'R'
                              WHEN 'Stand By'  THEN 'S'
                              WHEN 'Não Avaliado'  THEN 'N'
                              WHEN 'Não Avaliando' THEN 'N'
                              WHEN 'Nãoavalido'    THEN 'N'
                              ELSE status(1)
                     ).

  ENDMETHOD.

  METHOD setconvertqtd.

    DATA: convert TYPE c LENGTH 20.

    convert = qtd.
*    TRANSLATE CONVERT USING ',.'.

    CALL FUNCTION 'MOVE_CHAR_TO_NUM'
      EXPORTING
        chr             = convert
      IMPORTING
        num             = return
      EXCEPTIONS
        convt_no_number = 1
        convt_overflow  = 2
        OTHERS          = 3.


*    DATA: CONVERT TYPE C LENGTH 20,
*          NUMERO  TYPE N LENGTH 20.
*
*    CONVERT = QTD.
*
*    TRANSLATE CONVERT USING '. '.
*    TRANSLATE CONVERT USING ',.'.
*    CONDENSE CONVERT NO-GAPS.
*
*    WRITE CONVERT TO NUMERO.
*
*    RETURN = NUMERO.

  ENDMETHOD.

  METHOD setconvertdat.

    DATA convert TYPE char50.

    convert = data.

    TRANSLATE convert USING '/ '.
    TRANSLATE convert USING '- '.
    TRANSLATE convert USING ': '.
    TRANSLATE convert USING '. '.
    TRANSLATE convert USING 'T '.

    CONDENSE convert NO-GAPS.

    return = convert.

  ENDMETHOD.

  METHOD getseq.

    DATA: seq    TYPE numc15.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZSEQ0165'
      IMPORTING
        number                  = seq
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    return = seq.

*//ZSEQ0165
*    SELECT COUNT(*) INTO return FROM zsdt0165.
*    DATA(qtd) = REDUCE i( INIT x = 0 FOR ls IN it_0165 WHERE ( id IS NOT INITIAL ) NEXT x = x + 1 ).

*    ADD qtd TO return.
*    ADD 1 TO return.

  ENDMETHOD.

  METHOD getseq_0178.
    SELECT MAX( id ) INTO return FROM zsdt0178. ADD 1 TO return.
  ENDMETHOD.

  METHOD set_erros.

    DATA: it_log TYPE TABLE OF zsdt0178,
          atinn  TYPE atinn.

    IF input-contrato IS INITIAL.
      APPEND VALUE #( id_trace = input-id fieldname = 'CONTRATO' messagem = 'O campo Contrato está em Branco!' ) TO it_log.
    ELSE.
      DATA(wa) = zcl_tracecotton=>get0143( contrato = CONV #( input-contrato ) bukrs = lv_bukrs safra = input-safra ).
      IF wa-contrato IS INITIAL.
        APPEND VALUE #( id_trace = input-id fieldname = 'CONTRATO' messagem = |Não foi encontrado o contrato { input-contrato }! | ) TO it_log.
      ELSE.

        IF wa-cliente IS INITIAL.
          APPEND VALUE #( id_trace = input-id fieldname = 'CLIENTE' messagem = |Não foi encontrado cliente { wa-cliente } para o contrato { input-contrato }!| ) TO it_log.
        ENDIF.

        IF wa-preco IS INITIAL.
          APPEND VALUE #( id_trace = input-id fieldname = 'PRECO' messagem = |Não foi encontrado preço para o contrato { input-contrato }!| ) TO it_log.
        ENDIF.

      ENDIF.
    ENDIF.


    IF input-tipo IS INITIAL.
      APPEND VALUE #( id_trace = input-id fieldname = 'TIPO' messagem = 'O campo Tipo está em Branco!' ) TO it_log.
    ELSE.
      DATA(matnr) = zcl_tracecotton=>getmaterial( CONV #( input-tipo ) ).
      IF matnr IS INITIAL.
        APPEND VALUE #( id_trace = input-id fieldname = 'TIPO' messagem = |Não foi encontrado Material com o Tipo { input-tipo }!| ) TO it_log.
      ENDIF.
    ENDIF.

    IF input-safra IS INITIAL.
      APPEND VALUE #( id_trace = input-id fieldname = 'SAFRA' messagem = 'O campo Safra está em Branco!' ) TO it_log.
    ENDIF.

    DATA(algodoeira) = |{ input-algodoeira CASE = UPPER }|.

    IF algodoeira IS INITIAL.
      APPEND VALUE #( id_trace = input-id fieldname = 'ALGODOEIRA' messagem = 'O campo Algodoeira está em Branco!' ) TO it_log.
    ELSE.
      "DATA(werks) =  zcl_tracecotton=>getfazenda( algodoeira ).
      DATA(werks) =  input-codigo_filial."Projeto Reestruturação Algodao 2024
      IF werks IS INITIAL.
        "APPEND VALUE #( id_trace = input-id fieldname = 'ALGODOEIRA' messagem = |Não foi encontrado De/Para com o valor { algodoeira } na Transação ZSDT0140!| ) TO it_log. "Projeto Reestruturação Algodao 2024
        APPEND VALUE #( id_trace = input-id fieldname = 'ALGODOEIRA' messagem = |O Campo Id Filial Algodeira não foi informado!| ) TO it_log.  "Projeto Reestruturação Algodao 2024
      ELSE.

        IF input-lote IS INITIAL.
          APPEND VALUE #( id_trace = input-id fieldname = 'LOTE' messagem = 'O campo Lote está em Branco!' ) TO it_log.
        ELSE.
          DATA(charg) = input-safra && '_' && werks.
          DATA(wa_mchb) = get_mchb( matnr = matnr werks = werks lgort = CONV #( input-lote )  charg = CONV #( charg ) ).
*          SELECT COUNT(*) FROM MCHB WHERE MATNR EQ MATNR AND WERKS EQ WERKS AND LGORT EQ INPUT-LOTE.
          IF wa_mchb IS INITIAL.
            APPEND VALUE #( id_trace = input-id fieldname = 'LOTE' messagem = |O Bloco { input-lote } não possui saldo no Material { matnr }  Centro { werks } e Lote: { charg }!| ) TO it_log.
          ELSE.
*            DATA(OBJEK) = GET_MCH1( MATNR = WA_MCHB-MATNR CHARG = WA_MCHB-CHARG ).
*            IF GET_AUSP(
*                      OBJEK = CONV #( OBJEK )
*                      ATINN = GET_EXIT_ATINN_INPUT( 'SAFRA' )
*                    ) IS NOT INITIAL.
*            APPEND VALUE #( ID_TRACE = INPUT-ID FIELDNAME = 'LOTE' MESSAGEM = |O Lote { INPUT-LOTE } não está expandido para o Material { MATNR } e Centro { WERKS }!| ) TO IT_LOG.
*            ENDIF.
            "VALIDAR SE EXISTE ALGODOEIRA  VINCULADA ZPPT0004

            "Projeto Reestruturação Algodao 2024
*            IF algodoeira IS NOT INITIAL.
*              DATA(lv_algodoeira) =  zcl_tracecotton=>getalgodoeira( werks = werks lote =  input-lote   safra = input-safra ).
*              IF lv_algodoeira IS NOT INITIAL.
*                IF lv_algodoeira(1) EQ 'V' . "Caso retorno na posição 01 seja V retornou Verid para compor log de erro
*                  APPEND VALUE #( id_trace = input-id fieldname = 'ALGODOEIRA' messagem = |Não foi localizado a descrição da Algodoeira no centro { werks }, VERID { lv_algodoeira+1 } na transação ZMM0069.| ) TO it_log.
*                ENDIF.
*              ENDIF.
*            ENDIF.
            "Projeto Reestruturação Algodao 2024

          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.



    IF input-quantidade IS INITIAL.
      APPEND VALUE #( id_trace = input-id fieldname = 'QUANTIDADE' messagem = 'O campo Quantidade está em Branco!' ) TO it_log.
    ENDIF.

    IF input-peso IS INITIAL.
      APPEND VALUE #( id_trace = input-id fieldname = 'PESO' messagem = 'O campo Peso está em Branco!' ) TO it_log.
    ENDIF.

    IF input-tamanho_fardos IS INITIAL.
      APPEND VALUE #( id_trace = input-id fieldname = 'TAMANHO_FARDOS' messagem = 'O campo Tamanho dos Fados está em Branco!' ) TO it_log.
    ENDIF.

    IF input-data_realizacao IS INITIAL.
      APPEND VALUE #( id_trace = input-id fieldname = 'DATA_REALIZACAO' messagem = 'O campo Data do TakeUp está em Branco!' ) TO it_log.
    ENDIF.

    CASE input-status(1).
      WHEN 'A' OR 'R' OR 'S' OR 'N'.
      WHEN OTHERS.
        APPEND VALUE #( id_trace = input-id fieldname = 'STATUS' messagem = |Status "{ input-status }" não foi previsto!| ) TO it_log.
    ENDCASE.

    IF it_log IS NOT INITIAL.
      return =  abap_true.
      LOOP AT it_log INTO DATA(wa_log).
        wa_log-id         = zcl_tracecotton=>getseq_0178( ).
        wa_log-usnam      = sy-uname.
        wa_log-data_atual = sy-datum.
        wa_log-hora_atual = sy-uzeit.
        MODIFY zsdt0178 FROM wa_log.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD get_exit_atinn_input.
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = caracteristica
      IMPORTING
        output = return.
  ENDMETHOD.

  METHOD get_mchb.

    SELECT SINGLE *
      FROM mchb INTO return
     WHERE matnr EQ matnr
       AND werks EQ werks
       AND lgort EQ lgort
       AND charg EQ charg
       AND clabs > 0.
  ENDMETHOD.

  METHOD get_mch1.
    SELECT SINGLE cuobj_bm FROM mch1 INTO return WHERE matnr EQ matnr AND charg EQ charg.
  ENDMETHOD.

  METHOD get_ausp.
    DATA(safra) = get_exit_atinn_input( 'SAFRA' ).
    SELECT COUNT(*) FROM ausp
    WHERE objek EQ objek
      AND atinn EQ atinn
      AND atwrt EQ safra
      AND klart EQ '023'.
    return = sy-subrc.
  ENDMETHOD.


ENDCLASS.

INCLUDE lsvimdat                                . "general data decl.
INCLUDE lzsdtracecottont00                      . "view rel. data dcl.
