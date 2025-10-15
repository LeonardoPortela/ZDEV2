************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 23.02.2011                                          *
* Objetivo    ...: Atualização de status das notas de saida            *
* Job         ...: COMP_ATU_NOTAS                                      *
* Autor       ...: Victor Hugo                                         *
************************************************************************

REPORT  zlesj0002.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: zlest0039, zsdt0001.

*----------------------------------------------------------------------*
* ESTRUTURA DAS TABELAS
*----------------------------------------------------------------------*
TYPES:

  BEGIN OF ty_zlest0039,
    docnum         TYPE zlest0039-docnum,        " nOTA fISCAL
    vbeln          TYPE zlest0039-vbeln,        " Fornecimento
    status         TYPE zlest0039-status,       " Status comparativo saidas e chegadas
    unidadesaida   TYPE zlest0039-unidadesaida, " Unidade de medida básica
    datachegada    TYPE zlest0039-datachegada,  " Data de chegada
    cnpj           TYPE zlest0039-cnpj,         " CNPJ do cliente
    nfenum         TYPE zlest0039-nfenum,       " Nº NF-e de nove posições
    pontotransb    TYPE zlest0039-pontotransb,  " Ponto Transbordo
    pesotransb     TYPE zlest0039-pesotransb,   " Peso de Transbordo
    pesochegada    TYPE zlest0039-pesochegada,
    bukrs          TYPE zlest0039-bukrs,
    werks          TYPE zlest0039-werks,
    transb_efetivo TYPE zlest0039-transb_efetivo,
    datatransb     TYPE zlest0039-datatransb,
  END OF ty_zlest0039,

  BEGIN OF ty_zsdt0001,
    dt_chegada     TYPE zsdt0001-dt_chegada,
    peso_subtotal  TYPE zsdt0001-peso_subtotal,
    docdat         TYPE zsdt0001-docdat,       " Data do documento
    peso_fiscal    TYPE zsdt0001-peso_fiscal,  " Peso bruto
    tp_movimento   TYPE zsdt0001-tp_movimento, " Tipo de Movimento
    doc_rem        TYPE zsdt0001-doc_rem,      " Remessa AMAGGI
    dt_movimento   TYPE zsdt0001-dt_movimento,
    peso_liq       TYPE zsdt0001-peso_liq, " Peso Liquido
    local_descarga TYPE zsdt0001-local_descarga, "
    tp_transgenia  TYPE zsdt0001-tp_transgenia, "Tipo de Transgenia
  END OF ty_zsdt0001,

  BEGIN OF ty_zlest0019,
    dtachegada  TYPE zlest0019-dtachegada,  " Data de chegada
    pesodvagao  TYPE zlest0019-pesodvagao,  " Peso destinado ao vagão
    cnpjcliente TYPE zlest0019-cnpjcliente, " CNPJ do cliente
    tp_reg      TYPE zlest0019-tp_reg,      " Tipo de registro
    idinter     TYPE zlest0019-idinter,     " Identificador de interface ferroviário
    nfenum      TYPE zlest0019-nfenum,      " Nº NF-e de nove posições
    bukrs       TYPE zlest0019-bukrs,
    branch      TYPE zlest0019-branch,
  END OF ty_zlest0019,

  BEGIN OF ty_zlest0019_aux,
    pesodvagao TYPE zlest0019-pesodvagao,  " Peso destinado ao vagão
    tp_reg     TYPE zlest0019-tp_reg,      " Tipo de registro
    idinter    TYPE zlest0019-idinter,     " Identificador de interface ferroviário
    nfenum     TYPE zlest0019-nfenum,      " Nº NF-e de nove posições
  END OF ty_zlest0019_aux.

*----------------------------------------------------------------------*
* TABELAS INTERNAS
*----------------------------------------------------------------------*
DATA:
  it_zlest0039     TYPE TABLE OF ty_zlest0039,
  it_zsdt0001      TYPE TABLE OF ty_zsdt0001,
  it_zlest0019     TYPE TABLE OF ty_zlest0019,
  it_zlest0136     TYPE TABLE OF zlest0136 WITH HEADER LINE,
  it_zlest0173     TYPE TABLE OF zlest0173 WITH HEADER LINE,
  it_zlest0019_aux TYPE TABLE OF ty_zlest0019_aux.

*----------------------------------------------------------------------*
* WORK ÁREA
*----------------------------------------------------------------------*
DATA: wa_zlest0039     TYPE ty_zlest0039,
      wa_zsdt0001      TYPE ty_zsdt0001,
      ws_zsdt0001      TYPE ty_zsdt0001,
      wa_zlest0019     TYPE ty_zlest0019,
      wa_zlest0019_aux TYPE ty_zlest0019_aux.

*----------------------------------------------------------------------*
* PERFORM
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK z001 WITH FRAME.
  SELECT-OPTIONS: p_reme FOR zlest0039-vbeln.
SELECTION-SCREEN END OF BLOCK z001.


IF sy-batch EQ abap_true. "Se JOB MAGGI_ZLES0170 - Transação ZLES0170 estiver rodando, nao rodar esse JOB
  TRY.
      zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = 'ZLESR0149' IMPORTING e_qtd  = DATA(e_qtd) ).
    CATCH zcx_job.
  ENDTRY.

  IF e_qtd GT 1.
    LEAVE PROGRAM.
  ENDIF.
ENDIF.

SELECT SINGLE COUNT(*) INTO @DATA(vg_job)
    FROM tbtco
   WHERE jobname EQ 'ATUALIZA_COMP_J2'
     AND status EQ 'R'.

IF ( vg_job EQ 1 ).
  PERFORM: p_atualizacao_l1_intercompany.
ENDIF.
"p_atualizacao_l1_ferroviario.
*         p_atualizacao_l2_intercompany,
*         p_atualizacao_l2_ferroviario,
*         p_atualizacao_l3_intercompany,
*         p_atualizacao_l3_ferroviario.


*&---------------------------------------------------------------------*
*&      Form  P_ATUALIZACAO_L1
*&---------------------------------------------------------------------*
*   Essa atualização deverá ocorrer de duas formas.
*
*   Intercompany (Empresas do grupo):
*                Confirmação de chegada no ponto de transbordo através de um romaneio
*                de entrada enviado por interface pelo sistema OPUS-Recebimento
*&---------------------------------------------------------------------*
FORM p_atualizacao_l1_intercompany.

  DATA: wa_zlest0134     TYPE zlest0134,
        lc_datatransb    TYPE zlest0039-datatransb,
        lc_datasaida     TYPE zlest0039-datasaida,
        lc_dlydy         TYPE dlydy,
        lc_tp_transgenia TYPE zde_tp_transgenia,
        it_j_1bnfnad     TYPE STANDARD TABLE OF j_1bnfnad,
        ws_zsdt0001      TYPE zsdt0001,
        vg_peso_subtotal TYPE zde_nm_peso_subtotal.

  lc_dlydy = 90.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = sy-datum
      days      = lc_dlydy
      months    = 0
      years     = 0
      signum    = '-'
    IMPORTING
      calc_date = lc_datasaida.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = lc_datasaida
      days      = lc_dlydy
      months    = 0
      years     = 0
      signum    = '-'
    IMPORTING
      calc_date = lc_datasaida.

  SELECT  docnum vbeln status unidadesaida datachegada cnpj nfenum pontotransb pesotransb pesochegada bukrs werks transb_efetivo datatransb
     FROM zlest0039
     INTO TABLE it_zlest0039
   WHERE status EQ 'ET'
     AND vbeln  IN p_reme
     AND vbeln  NE space
     AND datasaida GE lc_datasaida
     AND ck_estornar_trans EQ abap_false.

  SELECT SINGLE * INTO wa_zlest0134 FROM zlest0134.
  IF sy-subrc IS INITIAL AND wa_zlest0134-qtd_dias_zlest0039 IS NOT INITIAL AND p_reme IS INITIAL.

    lc_datatransb = sy-datum.

    WHILE wa_zlest0134-qtd_dias_zlest0039 GT 0.

      IF wa_zlest0134-qtd_dias_zlest0039 GT 99.
        lc_dlydy = 99.
        ADD -99 TO wa_zlest0134-qtd_dias_zlest0039.
      ELSE.
        lc_dlydy = wa_zlest0134-qtd_dias_zlest0039.
        wa_zlest0134-qtd_dias_zlest0039 = 0.
      ENDIF.

      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = lc_datatransb
          days      = lc_dlydy
          months    = 0
          years     = 0
          signum    = '-'
        IMPORTING
          calc_date = lc_datatransb.
    ENDWHILE.

    SELECT  docnum vbeln status unidadesaida datachegada cnpj nfenum pontotransb pesotransb pesochegada bukrs werks
       FROM zlest0039
       APPENDING TABLE it_zlest0039
     WHERE status           EQ 'L1'
       AND datatransb       NE space
       AND datatransb       GE lc_datatransb
       AND tp_importacao_l1 NE 'A'.
  ENDIF.

  IF p_reme IS NOT INITIAL.
    SELECT  docnum vbeln status unidadesaida datachegada cnpj nfenum pontotransb pesotransb pesochegada bukrs werks
       FROM zlest0039
       APPENDING TABLE it_zlest0039
     WHERE status           EQ 'L1'
       AND vbeln            IN p_reme
       AND tp_importacao_l1 NE 'A'.
  ENDIF.

  CHECK NOT it_zlest0039[] IS INITIAL.

  SELECT dt_chegada peso_subtotal docdat peso_fiscal tp_movimento doc_rem dt_movimento peso_liq local_descarga tp_transgenia
    FROM zsdt0001
    INTO TABLE it_zsdt0001
    FOR ALL ENTRIES IN it_zlest0039
  WHERE doc_rem EQ it_zlest0039-vbeln
    AND tp_movimento = 'E'.

*  CHECK NOT IT_ZSDT0001[] IS INITIAL  "Comentado refernete ajuste IR125009 / aoenning.

  "Bucar Notas de Algodão com Dt de Chegada e Peso Chegada
  SELECT *
    INTO TABLE it_zlest0173
    FROM zlest0173
     FOR ALL ENTRIES IN it_zlest0039
   WHERE docnum        EQ it_zlest0039-docnum
     AND ch_referencia NE space.

  "Buscar Romaneios de Saida de Algodão
  IF it_zlest0173[] IS NOT INITIAL.
    SELECT * INTO TABLE @DATA(it_zsdt0001ovro)
      FROM zsdt0001ovro
       FOR ALL ENTRIES IN @it_zlest0173
     WHERE ch_referencia_sai EQ @it_zlest0173-ch_referencia.
    SORT it_zsdt0001ovro BY ch_referencia_sai.
  ENDIF.

  IF it_zsdt0001[] IS NOT INITIAL. "Ajuste realizado referente refernete ajuste IR125009 / aoenning.
    SELECT *
      INTO TABLE it_zlest0136
      FROM zlest0136
       FOR ALL ENTRIES IN it_zsdt0001
     WHERE local_descarga EQ it_zsdt0001-local_descarga.
  ENDIF.

  SORT: it_zlest0039 BY vbeln,
        it_zlest0173 BY doc_rem,
        it_zsdt0001  BY doc_rem,
        it_zlest0136 BY local_descarga.

  LOOP AT it_zlest0039 INTO wa_zlest0039.
    CLEAR: vg_peso_subtotal.

    "========================================================="Ajuste realizado referente refernete ajuste IR125009 / aoenning.
    READ TABLE it_zlest0173 INTO DATA(wa_zlest0173) WITH KEY doc_rem = wa_zlest0039-vbeln BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      READ TABLE it_zsdt0001ovro INTO DATA(wa_zsdt0001ovro) WITH KEY ch_referencia_sai = wa_zlest0173-ch_referencia.
      IF sy-subrc EQ 0.
        vg_peso_subtotal = wa_zsdt0001ovro-nm_peso_subtotal.
      ELSE.
        SELECT SINGLE * FROM zsdt0001 INTO ws_zsdt0001 WHERE ch_referencia EQ wa_zlest0173-ch_referencia.
        IF sy-subrc EQ 0.
          vg_peso_subtotal = ws_zsdt0001-peso_subtotal.
        ENDIF.
      ENDIF.

      IF vg_peso_subtotal IS INITIAL.
        CONTINUE.
      ENDIF.
      "========================================================="Ajuste realizado referente refernete ajuste IR125009 / aoenning.
      IF ( wa_zlest0039-pontotransb NE space ).

        IF wa_zsdt0001-tp_transgenia(1) = 'C'.
          lc_tp_transgenia = 'C'.
        ELSE.
          lc_tp_transgenia = 'T'.
        ENDIF.

        DATA: lw_zlest0039   TYPE zlest0039.
        DATA: lw_zlest0039_a TYPE zlest0039.
        MOVE-CORRESPONDING wa_zlest0039 TO lw_zlest0039.
        MOVE-CORRESPONDING wa_zlest0039 TO lw_zlest0039_a.
        lw_zlest0039_a-datatransb = wa_zlest0173-dt_chegada.
        lw_zlest0039_a-pesotransb = vg_peso_subtotal. "Ajuste realizado referente refernete ajuste IR125009 / aoenning.

        PERFORM verificar_carguero IN PROGRAM zlesi0005 USING lw_zlest0039 lw_zlest0039_a IF FOUND.
        CLEAR: lw_zlest0039_a, lw_zlest0039.

        CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
          EXPORTING
            docnum         = wa_zlest0039-docnum
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA

          UPDATE zlest0039
             SET datatransb       = wa_zlest0173-dt_chegada
                 pesotransb       = vg_peso_subtotal "Ajuste realizado referente refernete ajuste IR125009 / aoenning.
                 pesoliquido      = vg_peso_subtotal "Ajuste realizado referente refernete ajuste IR125009 / aoenning.
                 unidadetransb    = 'KG'
                 status           = 'L1'
                 tp_transgenia    = lc_tp_transgenia
                 dt_atualizacao   = sy-datum
                 us_atualizacao   = sy-uname
                 tp_importacao_l1 = 'R'
           WHERE vbeln            EQ wa_zlest0039-vbeln
             AND tp_importacao_l1 NE 'A'.

          CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
            EXPORTING
              docnum = wa_zlest0039-docnum.

        ENDIF.

      ELSE.

        MOVE-CORRESPONDING wa_zlest0039 TO lw_zlest0039.
        MOVE-CORRESPONDING wa_zlest0039 TO lw_zlest0039_a.
        lw_zlest0039_a-datachegada = wa_zlest0173-dt_chegada.
        lw_zlest0039_a-pesochegada = vg_peso_subtotal. "Ajuste realizado referente refernete ajuste IR125009 / aoenning.

        PERFORM verificar_carguero IN PROGRAM zlesi0005 USING lw_zlest0039 lw_zlest0039_a IF FOUND.
        CLEAR: lw_zlest0039_a, lw_zlest0039.

        CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
          EXPORTING
            docnum         = wa_zlest0039-docnum
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA

          UPDATE zlest0039
             SET datachegada      = wa_zlest0173-dt_chegada
                 pesochegada      = vg_peso_subtotal "Ajuste realizado referente refernete ajuste IR125009 / aoenning.
                 pesoliquido      = vg_peso_subtotal "Ajuste realizado referente refernete ajuste IR125009 / aoenning.
                 unidadechegada   = 'KG'
                 status           = 'L1'
                 tp_transgenia    = lc_tp_transgenia
                 dt_atualizacao   = sy-datum
                 us_atualizacao   = sy-uname
                 tp_importacao_l1 = 'R'
           WHERE vbeln EQ wa_zlest0039-vbeln
             AND tp_importacao_l1 NE 'A'.

        ENDIF.
      ENDIF.

    ELSE.

      READ TABLE it_zsdt0001 INTO wa_zsdt0001 WITH KEY doc_rem = wa_zlest0039-vbeln BINARY SEARCH.
      IF ( sy-subrc = 0 ).

        IF wa_zsdt0001-local_descarga IS NOT INITIAL.
          READ TABLE it_zlest0136 WITH KEY local_descarga = wa_zsdt0001-local_descarga BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            wa_zlest0039-transb_efetivo = it_zlest0136-parceiro_lr.
          ELSE.
            IF wa_zsdt0001-local_descarga = '76'.
              wa_zlest0039-transb_efetivo = '0000001003'.
            ELSEIF wa_zsdt0001-local_descarga = '257'.
              wa_zlest0039-transb_efetivo = '0000000161'.
            ELSEIF wa_zsdt0001-local_descarga = '278'.
              wa_zlest0039-transb_efetivo = '0000004102'.
            ELSEIF wa_zsdt0001-local_descarga = '258'.
              wa_zlest0039-transb_efetivo = '0000003802'.
            ENDIF.
          ENDIF.
        ENDIF.

        "Somente Fazer o L1 quando carga for direto para o local 258 senão o romaneio será determinação para o L3.
        IF wa_zsdt0001-local_descarga EQ '258'.
          DATA: rg_parc TYPE RANGE OF j_1bparvw.
          rg_parc = VALUE #( sign = 'I' option = 'EQ' ( low = 'LR' ) ( low = 'Z1' ) ).

          SELECT * INTO TABLE it_j_1bnfnad
            FROM j_1bnfnad
           WHERE docnum EQ wa_zlest0039-docnum
             AND parvw  IN rg_parc.

          READ TABLE it_j_1bnfnad INTO DATA(wa_lr) WITH KEY parvw = 'LR'.
          READ TABLE it_j_1bnfnad INTO DATA(wa_z1) WITH KEY parvw = 'Z1'.
          IF wa_lr-cgc NE wa_z1-cgc.
            "Romaneio de Entrada no Destino Final
            CONTINUE.
          ENDIF.
        ENDIF.

        IF ( wa_zlest0039-pontotransb NE space ).

          IF wa_zsdt0001-tp_transgenia(1) = 'C'.
            lc_tp_transgenia = 'C'.
          ELSE.
            lc_tp_transgenia = 'T'.
          ENDIF.

          MOVE-CORRESPONDING wa_zlest0039 TO lw_zlest0039.
          MOVE-CORRESPONDING wa_zlest0039 TO lw_zlest0039_a.
          lw_zlest0039_a-datatransb  = wa_zsdt0001-dt_movimento.
          lw_zlest0039_a-pesotransb  = wa_zsdt0001-peso_subtotal.
          PERFORM verificar_carguero IN PROGRAM zlesi0005 USING lw_zlest0039 lw_zlest0039_a IF FOUND.
          CLEAR: lw_zlest0039_a, lw_zlest0039.


          CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
            EXPORTING
              docnum         = wa_zlest0039-docnum
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA

            UPDATE zlest0039
               SET datatransb       = wa_zsdt0001-dt_movimento
                   pesotransb       = wa_zsdt0001-peso_subtotal
                   pesoliquido      = wa_zsdt0001-peso_liq
                   unidadetransb    = wa_zlest0039-unidadesaida
                   status           = 'L1'
                   transb_efetivo   = wa_zlest0039-transb_efetivo
                   tp_transgenia    = lc_tp_transgenia
                   dt_atualizacao   = sy-datum
                   us_atualizacao   = sy-uname
                   tp_importacao_l1 = 'R'
             WHERE vbeln            EQ wa_zlest0039-vbeln
               AND tp_importacao_l1 NE 'A'.

            CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
              EXPORTING
                docnum = wa_zlest0039-docnum.

          ENDIF.
        ELSE.

          MOVE-CORRESPONDING wa_zlest0039 TO lw_zlest0039.
          MOVE-CORRESPONDING wa_zlest0039 TO lw_zlest0039_a.
          lw_zlest0039_a-datachegada = wa_zsdt0001-dt_movimento.
          lw_zlest0039_a-pesochegada = wa_zsdt0001-peso_subtotal.
          PERFORM verificar_carguero IN PROGRAM zlesi0005 USING lw_zlest0039 lw_zlest0039_a IF FOUND.
          CLEAR: lw_zlest0039_a, lw_zlest0039.

          CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
            EXPORTING
              docnum         = wa_zlest0039-docnum
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA

            UPDATE zlest0039
               SET datachegada      =  wa_zsdt0001-dt_movimento
                   pesochegada      =  wa_zsdt0001-peso_subtotal
                   pesoliquido      = wa_zsdt0001-peso_liq
                   unidadechegada   =  wa_zlest0039-unidadesaida
                   status           = 'L1'
                   transb_efetivo   = wa_zlest0039-transb_efetivo
                   tp_transgenia    = lc_tp_transgenia
                   dt_atualizacao   = sy-datum
                   us_atualizacao   = sy-uname
                   tp_importacao_l1 = 'R'
             WHERE vbeln EQ wa_zlest0039-vbeln
               AND tp_importacao_l1 NE 'A'.

            CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
              EXPORTING
                docnum = wa_zlest0039-docnum.

          ENDIF.
        ENDIF.
      ELSE.
        IF ( wa_zlest0039-pontotransb NE space ).

          MOVE-CORRESPONDING wa_zlest0039 TO lw_zlest0039.
          MOVE-CORRESPONDING wa_zlest0039 TO lw_zlest0039_a.
          CLEAR: lw_zlest0039_a-datatransb, lw_zlest0039_a-pesotransb.
          PERFORM verificar_carguero IN PROGRAM zlesi0005 USING lw_zlest0039 lw_zlest0039_a IF FOUND.
          CLEAR: lw_zlest0039_a, lw_zlest0039.


          CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
            EXPORTING
              docnum         = wa_zlest0039-docnum
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA

            UPDATE zlest0039
               SET datatransb     = space
                   pesotransb     = 0
                   pesoliquido    = 0
                   unidadetransb  = space
                   status         = 'ET'
                   transb_efetivo = space
                   tp_transgenia  = space
                   dt_atualizacao = sy-datum
                   us_atualizacao = sy-uname
             WHERE vbeln EQ wa_zlest0039-vbeln
               AND tp_importacao_l1 NE 'A'.


            CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
              EXPORTING
                docnum = wa_zlest0039-docnum.

          ENDIF.
        ELSE.

          MOVE-CORRESPONDING wa_zlest0039 TO lw_zlest0039.
          MOVE-CORRESPONDING wa_zlest0039 TO lw_zlest0039_a.
          CLEAR: lw_zlest0039_a-datachegada, lw_zlest0039_a-pesochegada.
          PERFORM verificar_carguero IN PROGRAM zlesi0005 USING lw_zlest0039 lw_zlest0039_a IF FOUND.
          CLEAR: lw_zlest0039_a, lw_zlest0039.

          CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
            EXPORTING
              docnum         = wa_zlest0039-docnum
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA

            UPDATE zlest0039
               SET datachegada    = space
                   pesochegada    = 0
                   pesoliquido    = 0
                   unidadechegada = space
                   status         = 'ET'
                   transb_efetivo = space
                   tp_transgenia  = space
                   dt_atualizacao = sy-datum
                   us_atualizacao = sy-uname
             WHERE vbeln EQ wa_zlest0039-vbeln
               AND tp_importacao_l1 NE 'A'.


            CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
              EXPORTING
                docnum = wa_zlest0039-docnum.

          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.
    CLEAR: wa_zlest0039,
           wa_zsdt0001.
  ENDLOOP.

  "FF #178839 - inicio
  IF it_zlest0039[] IS NOT INITIAL.

    SELECT * FROM zlest0039
    FOR ALL ENTRIES IN @it_zlest0039
    WHERE docnum = @it_zlest0039-docnum
      AND status = 'L1'
      AND tp_importacao_l1  IS NOT INITIAL
    INTO TABLE @DATA(lt_0039).

    IF sy-subrc = 0.

      DATA lt_0035 TYPE TABLE OF zlest0035.

      LOOP AT lt_0039 INTO DATA(wa_0039).

* ----> BUG #180372 - MMSILVA - 26.05.2025 - Inicio <----
        WHILE strlen( wa_0039-serie ) < 3.
          wa_0039-serie = |0{ wa_0039-serie }|.
        ENDWHILE.
* ----> BUG #180372 - MMSILVA - 26.05.2025 - Fim <----

        APPEND VALUE #(
          nr_nf        = wa_0039-nfenum
          serie_nf     = wa_0039-serie
          cnpj         = wa_0039-cnpj
          docnum       = wa_0039-docnum
          werks        = wa_0039-werks
          qtd_nf       = wa_0039-pesosaida
          qtd_cheg     = wa_0039-pesotransb
          dtachegada   = wa_0039-datatransb
          saldo        = wa_0039-pesotransb
        ) TO lt_0035.

      ENDLOOP.

      TRY.
          MODIFY zlest0035 FROM TABLE @lt_0035.
        CATCH cx_sy_open_sql_db INTO DATA(lx_sql).
          MESSAGE lx_sql->get_text( ) TYPE 'W'.
      ENDTRY.

      IF sy-subrc <> 0.

      ENDIF.
    ENDIF.
  ENDIF.
  "FF #178839 - fim


ENDFORM.                    "p_atualizacao_l1_intercompany
*&---------------------------------------------------------------------*
*&      Form  P_ATUALIZACAO_L1_FERROVIARIO
*&---------------------------------------------------------------------*
*
*   Terceiros (Ferroviario):
*               Confirmação de chegada no ponto de transbordo através de um arquivo fornecido
*               por um fornecedor com os dados das notas de saida descarregadas no terminal de transbordo
*&---------------------------------------------------------------------*
FORM p_atualizacao_l1_ferroviario.

  CLEAR: it_zlest0039, wa_zlest0039.
  DATA : wa_j_1bbranch TYPE j_1bbranch.

  " SELECT  vbeln status unidadesaida datachegada cnpj nfenum pontotransb pesotransb pesochegada bukrs werks transb_efetivo datatransb
  SELECT docnum
         vbeln
         status
         unidadesaida
         datachegada
         cnpj
         nfenum
         pontotransb
         pesotransb
         pesochegada
         bukrs
         werks
         transb_efetivo
         datatransb
     FROM zlest0039
     INTO TABLE it_zlest0039
   WHERE status EQ 'ET'.

  CHECK NOT it_zlest0039[] IS INITIAL.

  SELECT dtachegada pesodvagao cnpjcliente tp_reg idinter nfenum bukrs branch
    FROM zlest0019
    INTO TABLE it_zlest0019
    FOR ALL ENTRIES IN it_zlest0039
  WHERE nfenum EQ it_zlest0039-nfenum
    AND bukrs  EQ it_zlest0039-bukrs
    AND branch EQ it_zlest0039-werks
    AND tp_reg  = '30'
    AND idinter = 'L1'.

  CHECK NOT it_zlest0019[] IS INITIAL.

  SORT: it_zlest0039 BY nfenum bukrs werks,
        it_zlest0019 BY nfenum bukrs branch.

  LOOP AT it_zlest0039 INTO wa_zlest0039.

    READ TABLE it_zlest0019 INTO wa_zlest0019 WITH KEY nfenum = wa_zlest0039-nfenum
                                                       bukrs  = wa_zlest0039-bukrs
                                                       branch = wa_zlest0039-werks
                                              BINARY SEARCH.



    IF ( sy-subrc = 0 ).
      IF ( wa_zlest0039-pontotransb NE space ).

        DATA: lw_zlest0039   TYPE zlest0039.
        DATA: lw_zlest0039_a TYPE zlest0039.
        MOVE-CORRESPONDING wa_zlest0039 TO lw_zlest0039.
        MOVE-CORRESPONDING wa_zlest0039 TO lw_zlest0039_a.
        lw_zlest0039_a-datatransb = wa_zlest0019-dtachegada.
        lw_zlest0039_a-pesotransb = wa_zlest0019-pesodvagao.
        PERFORM verificar_carguero IN PROGRAM zlesi0005 USING lw_zlest0039 lw_zlest0039_a IF FOUND.
        CLEAR: lw_zlest0039_a, lw_zlest0039.

        CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
          EXPORTING
            docnum         = wa_zlest0039-docnum
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA

          UPDATE zlest0039
            SET datatransb     = wa_zlest0019-dtachegada
                pesotransb     = wa_zlest0019-pesodvagao
                unidadetransb  = wa_zlest0039-unidadesaida
                status         = 'L1'
         WHERE vbeln EQ wa_zlest0039-vbeln.


          CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
            EXPORTING
              docnum = wa_zlest0039-docnum.

        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR: wa_zlest0039,
           wa_zlest0019.
  ENDLOOP.
ENDFORM.                    " P_ATUALIZACAO_L1

*&---------------------------------------------------------------------*
*&      Form  P_ATUALIZACAO_L2
*&---------------------------------------------------------------------*
*   Essa atualização deverá ocorrer de duas formas.
*
*   Intercompany (Empresas do Grupo):
*                Confirmação de saida do ponto de transbordo através de um
*                documento de transporte aquaviário registrado no sistema SAP
*&---------------------------------------------------------------------*

" Essa parte está em desenvolvimento, por isso só tem o select do Ferroviario, abaixo.

*&---------------------------------------------------------------------*
*&      Form  p_atualizacao_l2_INTERCOMPANY
*&---------------------------------------------------------------------*
FORM p_atualizacao_l2_intercompany.

ENDFORM.                    "p_atualizacao_l2_INTERCOMPANY

*&---------------------------------------------------------------------*
*&      Form  p_atualizacao_l2_ferroviario
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*   Terceiros (Ferroviario:
*               Confirmação de chegada no ponto de transbordo através de um arquivo
*               fornecido por um fornecedor com os dados das notas de saida descarregadas
*               no terminal de transbordo
*----------------------------------------------------------------------*
FORM p_atualizacao_l2_ferroviario.
*   Terceiros (Ferroviario:
*               Confirmação de chegada no ponto de transbordo através de um arquivo
*               fornecido por um fornecedor com os dados das notas de saida descarregadas
*               no terminal de transbordo

  CLEAR: it_zlest0039, it_zlest0019.
*---> 31/05/2023 - Migração S4 - JS
  "SELECT vbeln status unidadesaida datachegada cnpj nfenum pontotransb pesotransb pesochegada bukrs werks transb_efetivo datatransb
  SELECT docnum
         vbeln
         status
         unidadesaida
         datachegada
         cnpj
         nfenum
         pontotransb
         pesotransb
         pesochegada
         bukrs
         werks
         transb_efetivo
         datatransb
*<--- 31/05/2023 - Migração S4 - JS
    FROM zlest0039
    INTO TABLE it_zlest0039
  WHERE status EQ 'L1'.

  CHECK NOT it_zlest0039[] IS INITIAL.

  SELECT dtachegada pesodvagao cnpjcliente tp_reg idinter nfenum bukrs branch
    FROM zlest0019
    INTO TABLE it_zlest0019
    FOR ALL ENTRIES IN it_zlest0039
  WHERE nfenum EQ it_zlest0039-nfenum
    AND bukrs  EQ it_zlest0039-bukrs
    AND branch EQ it_zlest0039-werks
    AND tp_reg = '30'
    AND idinter = 'L2'.

  CHECK NOT it_zlest0019[] IS INITIAL.

  LOOP AT it_zlest0019 INTO wa_zlest0019.
    MOVE wa_zlest0019-pesodvagao TO wa_zlest0019_aux-pesodvagao.
    MOVE wa_zlest0019-nfenum     TO wa_zlest0019_aux-nfenum.
    COLLECT wa_zlest0019_aux INTO it_zlest0019_aux.
  ENDLOOP.

  SORT: it_zlest0039 BY nfenum cnpj,
        it_zlest0019 BY nfenum cnpjcliente.

  LOOP AT it_zlest0039 INTO wa_zlest0039.

    READ TABLE it_zlest0019 INTO wa_zlest0019 WITH KEY nfenum = wa_zlest0039-nfenum
                                                       bukrs  = wa_zlest0039-bukrs
                                                       branch = wa_zlest0039-werks
                                                       BINARY SEARCH.

    READ TABLE it_zlest0019_aux INTO wa_zlest0019_aux WITH KEY nfenum = wa_zlest0039-nfenum BINARY SEARCH.

    IF ( sy-subrc = 0 ).

      CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
        EXPORTING
          docnum         = wa_zlest0039-docnum
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA

        UPDATE zlest0039
          SET dataterminal    = wa_zlest0019-dtachegada
              pesoterminal    = wa_zlest0019_aux-pesodvagao
              unidadeterminal = wa_zlest0039-unidadesaida
              status         = 'L2'
        WHERE vbeln EQ wa_zlest0039-vbeln.


        CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
          EXPORTING
            docnum = wa_zlest0039-docnum.

      ENDIF.

    ENDIF.

    CLEAR: wa_zlest0039,
           wa_zlest0019.
  ENDLOOP.
ENDFORM.                    " P_ATUALIZACAO_L2

*&---------------------------------------------------------------------*
*&      Form  P_ATUALIZACAO_L3
*&---------------------------------------------------------------------*
*   Atualização L3 (Produto Entregue)
*   Essa atualização deverá ocorrer de duas formas:
*
*   Intercompany (Empresas do Grupo):
*              confirmação de chegada através  de um romaneio de entrada
*              enviado por interface pelo sistema OPUS-Recebimento.
*&---------------------------------------------------------------------*
FORM p_atualizacao_l3_intercompany.

  CLEAR: it_zlest0039, it_zsdt0001.

*---> 31/05/2023 - Migração S4 - JS
*SELECT vbeln status unidadesaida datachegada cnpj nfenum pontotransb pesotransb pesochegada bukrs werks transb_efetivo datatransb
  SELECT
  docnum
  vbeln
  status
  unidadesaida
  datachegada
  cnpj
  nfenum
  pontotransb
  pesotransb
  pesochegada
  bukrs
  werks
  transb_efetivo
  datatransb
*<--- 31/05/2023 - Migração S4 - JS
    FROM zlest0039
    INTO TABLE it_zlest0039
  WHERE status IN ('L1','L2')
    AND datachegada NE 0
    AND vbeln NE space.

  CHECK NOT it_zlest0039[] IS INITIAL.

  SELECT dt_chegada peso_subtotal docdat peso_fiscal tp_movimento doc_rem dt_movimento peso_liq
    FROM zsdt0001
    INTO TABLE it_zsdt0001
    FOR ALL ENTRIES IN it_zlest0039
  WHERE doc_rem EQ it_zlest0039-vbeln
    AND tp_movimento = 'E'.

  CHECK NOT it_zsdt0001[] IS INITIAL.

  SORT: it_zlest0039 BY vbeln,
        it_zsdt0001 BY doc_rem.

  LOOP AT it_zlest0039 INTO wa_zlest0039.
    READ TABLE it_zsdt0001 INTO wa_zsdt0001 WITH KEY doc_rem = wa_zlest0039-vbeln BINARY SEARCH.

    " Data chegada não pode estar NULA.
    IF ( sy-subrc = 0 ).

      IF  ( wa_zlest0039-pesochegada NE 0 ).

        CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
          EXPORTING
            docnum         = wa_zlest0039-docnum
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA

          UPDATE zlest0039
           SET   status         = 'L3'
                  pesoliquido   = wa_zsdt0001-peso_liq
           WHERE vbeln EQ wa_zlest0039-vbeln.


          CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
            EXPORTING
              docnum = wa_zlest0039-docnum.

        ENDIF.
      ELSE.

        CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
          EXPORTING
            docnum         = wa_zlest0039-docnum
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA

          UPDATE zlest0039
           SET datachegada    = wa_zsdt0001-dt_chegada
               pesochegada    = wa_zsdt0001-peso_subtotal
               pesoliquido   = wa_zsdt0001-peso_liq
               unidadechegada = wa_zlest0039-unidadesaida
               status         = 'L3'
           WHERE vbeln EQ wa_zlest0039-vbeln.


          CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
            EXPORTING
              docnum = wa_zlest0039-docnum.

        ENDIF.
      ENDIF.
      CLEAR: wa_zlest0039,
             ws_zsdt0001,
             wa_zsdt0001.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "p_atualizacao_l3_intercompany

*&---------------------------------------------------------------------*
*&      Form  P_ATUALIZACAO_L3
*&---------------------------------------------------------------------*
*   Terceiros (Ferroviario):
*                confirmação de chegada no ponto de entrega (Terminal Porto)
*                através  de um arquivo fornecido por um fornecedor com os dados
*                das notas de saída descarregadas  no terminal de descarga.
*
*----------------------------------------------------------------------*
FORM p_atualizacao_l3_ferroviario.

*----------------------------------------------------------------------*
*   Atualização L3 (Produto Entregue)
*   Terceiros - confirmação de chegada no ponto de entrega (Terminal Porto)
*                através  de um arquivo fornecido por um fornecedor com os dados
*                das notas de saída descarregadas  no terminal de descarga.
*----------------------------------------------------------------------*
  CLEAR: it_zlest0039, it_zlest0019.
*---> 31/05/2023 - Migração S4 - JS
*SELECT vbeln status unidadesaida datachegada cnpj nfenum pontotransb pesotransb pesochegada bukrs werks transb_efetivo datatransb
  SELECT
docnum
vbeln
status
unidadesaida
datachegada
cnpj
nfenum
pontotransb
pesotransb
pesochegada
bukrs
werks
transb_efetivo
datatransb
*<--- 31/05/2023 - Migração S4 - JS
   FROM zlest0039
   INTO TABLE it_zlest0039
 WHERE status EQ 'L2'
   AND nfenum NE space.

  CHECK NOT it_zlest0039[] IS INITIAL.

  SELECT dtachegada pesodvagao cnpjcliente tp_reg idinter nfenum bukrs branch
    FROM zlest0019
    INTO TABLE it_zlest0019
    FOR ALL ENTRIES IN it_zlest0039
  WHERE nfenum EQ it_zlest0039-nfenum
    AND bukrs  EQ it_zlest0039-bukrs
    AND branch EQ it_zlest0039-werks
    AND tp_reg = '30'
    AND idinter = 'L3'.

  CHECK NOT it_zlest0019[] IS INITIAL.

  SORT: it_zlest0039 BY nfenum,
        it_zlest0019 BY nfenum.

  LOOP AT it_zlest0039 INTO wa_zlest0039.
    READ TABLE it_zlest0019 INTO wa_zlest0019 WITH KEY nfenum = wa_zlest0039-nfenum
                                                       bukrs  = wa_zlest0039-bukrs
                                                       branch = wa_zlest0039-werks
                                                       BINARY SEARCH.

    IF ( sy-subrc = 0 ).

      CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
        EXPORTING
          docnum         = wa_zlest0039-docnum
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA

        UPDATE zlest0039
          SET datachegada    = wa_zlest0019-dtachegada
              pesochegada    = wa_zlest0019-pesodvagao
              unidadechegada = wa_zlest0039-unidadesaida
              status         = 'L3'
        WHERE vbeln EQ wa_zlest0039-vbeln.

        CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
          EXPORTING
            docnum = wa_zlest0039-docnum.

      ENDIF.
    ENDIF.

    CLEAR: wa_zlest0039,
           wa_zlest0019.
  ENDLOOP.
ENDFORM.                    " P_ATUALIZACAO_L3
