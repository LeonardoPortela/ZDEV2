************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 09.08.2011                                          *
* Objetivo    ...: Job de Validação - Comparativo de Saída e Chegada   *
************************************************************************

REPORT  zlesj0003.

TABLES: zlest0039.

*----------------------------------------------------------------------*
* ESTRUTURA DAS TABELAS
*----------------------------------------------------------------------*
TYPES:

  BEGIN OF ty_zlest0039,
    datasaida     TYPE zlest0039-datasaida,
    placa_cav     TYPE zlest0039-placa_cav,
    status_placa  TYPE zlest0039-status_placa,
    dias_transito TYPE zlest0039-dias_transito,
    pontotransb   TYPE zlest0039-pontotransb,
    datatransb    TYPE zlest0039-datatransb,
    datachegada   TYPE zlest0039-datachegada,
    vbeln         TYPE zlest0039-vbeln,
    ebeln         TYPE zlest0039-ebeln,

  END OF ty_zlest0039.

*----------------------------------------------------------------------*
* TABELAS INTERNAS
*----------------------------------------------------------------------*
DATA:
  it_zlest0039                TYPE TABLE OF ty_zlest0039,
  it_zlest0039_duplicacao     TYPE TABLE OF ty_zlest0039,
  it_zlest0039_duplicacao_aux TYPE TABLE OF ty_zlest0039,
  it_zlest0039_corrigido      TYPE TABLE OF ty_zlest0039,
  it_zlest0039_transito       TYPE TABLE OF ty_zlest0039.

*----------------------------------------------------------------------*
* WORK ÁREA
*----------------------------------------------------------------------*
DATA:
  wa_zlest0039                TYPE ty_zlest0039,
  wa_zlest0039_duplicacao     TYPE ty_zlest0039,
  wa_zlest0039_duplicacao_aux TYPE ty_zlest0039,
  wa_zlest0039_corrigido      TYPE ty_zlest0039,
  wa_zlest0039_transito       TYPE ty_zlest0039.

*----------------------------------------------------------------------*
* VARIAVEIS
*----------------------------------------------------------------------*
DATA: diatras TYPE p,   " Data Chegada - Data Saída
      s_dias  TYPE p.   " Data Chegada - Data Saída

*----------------------------------------------------------------------*
* PERFORM
*----------------------------------------------------------------------*

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
   WHERE jobname EQ 'ATUALIZA_COMP_J3'
     AND status EQ 'R'.

IF ( vg_job EQ 1 ).
  PERFORM:
          z_validacao_placa.
ENDIF.
*        z_validacao_dias_transito,
*        z_validacao_dias_corrigido.



*&---------------------------------------------------------------------*
*&      Form  Z_VALIDACAO_PLACA
*&---------------------------------------------------------------------*
*& Descrição:
*&  Validação para duplicação da placa do cavalo.
*& 0 = Não verificado
*& 1 = Não Aplicado
*& 2 = Duplicada
*& 3 = Alterado
*&---------------------------------------------------------------------*
FORM z_validacao_placa .

  DATA: qtd TYPE p.

  SELECT datasaida placa_cav status_placa dias_transito pontotransb datatransb datachegada
   FROM zlest0039
   INTO TABLE it_zlest0039_duplicacao
  WHERE status_placa EQ space.

  CHECK NOT it_zlest0039_duplicacao[] IS INITIAL.

  LOOP AT it_zlest0039_duplicacao INTO wa_zlest0039_duplicacao WHERE status_placa EQ space.
    APPEND wa_zlest0039_duplicacao TO it_zlest0039_duplicacao_aux.
  ENDLOOP.

  SORT: it_zlest0039_duplicacao BY placa_cav datasaida.

  LOOP AT it_zlest0039_duplicacao INTO wa_zlest0039_duplicacao.
    qtd = 0.
    LOOP AT it_zlest0039_duplicacao_aux INTO wa_zlest0039_duplicacao_aux WHERE placa_cav EQ wa_zlest0039_duplicacao-placa_cav
                                                                          AND datasaida  EQ wa_zlest0039_duplicacao-datasaida.
      qtd = qtd + 1.
      IF ( qtd >= 2 ).

        "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
        SELECT SINGLE docnum FROM zlest0039 WHERE placa_cav EQ @wa_zlest0039_duplicacao_aux-placa_cav AND datasaida EQ @wa_zlest0039_duplicacao_aux-datasaida INTO @DATA(_docnum).

        IF sy-subrc eq 0 AND _docnum IS NOT INITIAL.
          CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
            EXPORTING
              docnum         = _docnum
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
            UPDATE zlest0039 SET status_placa = 2 WHERE placa_cav EQ wa_zlest0039_duplicacao_aux-placa_cav
                                                    AND datasaida EQ wa_zlest0039_duplicacao_aux-datasaida.
            CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
              EXPORTING
                docnum = _docnum.

          ENDIF.

        ENDIF.
      ENDIF.
    ENDLOOP.

    IF ( qtd <= 1 ).

      "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
      SELECT SINGLE docnum FROM zlest0039 WHERE placa_cav EQ @wa_zlest0039_duplicacao-placa_cav AND datasaida EQ @wa_zlest0039_duplicacao-datasaida INTO @_docnum.

      IF sy-subrc eq 0 AND _docnum IS NOT INITIAL.
        CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
          EXPORTING
            docnum         = _docnum
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA

          UPDATE zlest0039 SET status_placa = 1 WHERE placa_cav EQ wa_zlest0039_duplicacao-placa_cav
                                                AND datasaida EQ wa_zlest0039_duplicacao-datasaida.
          CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
            EXPORTING
              docnum = _docnum.

        ENDIF.

      ENDIF.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " Z_VALIDACAO_PLACA
*&---------------------------------------------------------------------*
*&      Form  Z_VALIDACAO_DIAS_TRANSITO
*&---------------------------------------------------------------------*
*& Descrição:
*&  Validação de dias em transito
*& 0 = Normal
*& 1 = 1 até 4 dias
*& 2 = 5 até 7 dias
*& 3 = Maior ou igual 8 dias
*&---------------------------------------------------------------------*
FORM z_validacao_dias_transito.
*---> 01/06/2023 - Migração S4 - JS
* SELECT DATASAIDA PLACA_CAV STATUS_PLACA PONTOTRANSB DATATRANSB DATACHEGADA DIAS_TRANSITO VBELN EBELN
  SELECT datasaida
         placa_cav
         status_placa
         dias_transito
         pontotransb
         datatransb
         datachegada
         vbeln
         ebeln
*<--- 01/06/2023 - Migração S4 - JS
    FROM zlest0039
    INTO TABLE it_zlest0039
  WHERE datachegada NE space.

  CHECK NOT it_zlest0039[] IS INITIAL.

  LOOP AT it_zlest0039 INTO wa_zlest0039.

    CLEAR: diatras, s_dias.

    IF ( wa_zlest0039-pontotransb NE space ).
      diatras = wa_zlest0039-datatransb - wa_zlest0039-datasaida.
    ELSE.
      diatras = wa_zlest0039-datachegada - wa_zlest0039-datasaida.
    ENDIF.

    IF  ( diatras >= 1  AND diatras <= 4 ) .
      s_dias = 1.
    ELSEIF ( diatras >= 5 AND diatras <= 7 ).
      s_dias = 2.
    ELSE.
      s_dias = 3.
    ENDIF.

    "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
    SELECT SINGLE docnum FROM zlest0039
    WHERE datasaida = @wa_zlest0039-datasaida
    AND placa_cav   = @wa_zlest0039-placa_cav
    AND vbeln       = @wa_zlest0039-vbeln
    AND ebeln       = @wa_zlest0039-ebeln
    AND datachegada NE '00000000'
    INTO @DATA(_docnum).

    IF _docnum IS NOT INITIAL.
      CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
        EXPORTING
          docnum         = _docnum
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
        UPDATE zlest0039 SET dias_transito = s_dias WHERE datasaida   EQ wa_zlest0039-datasaida
                                            AND placa_cav   EQ wa_zlest0039-placa_cav
                                            AND vbeln       EQ wa_zlest0039-vbeln
                                            AND ebeln       EQ wa_zlest0039-ebeln
                                            AND datachegada NE space.
        CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
          EXPORTING
            docnum = _docnum.

      ENDIF.

    ENDIF.

    CLEAR: wa_zlest0039.
  ENDLOOP.
ENDFORM.                    " Z_VALIDACAO_DIAS_TRANSITO
*&---------------------------------------------------------------------*
*&      Form  Z_VALIDACAO_DIAS_CORRIGIDO
*&---------------------------------------------------------------------*
FORM z_validacao_dias_corrigido .
*---> 01/06/2023 - Migração S4 - JS
* SELECT DATASAIDA PLACA_CAV STATUS_PLACA PONTOTRANSB DATATRANSB DATACHEGADA DIAS_TRANSITO VBELN EBELN
  SELECT datasaida
         placa_cav
         status_placa
         dias_transito
         pontotransb
         datatransb
         datachegada
         vbeln
         ebeln
*<--- 01/06/2023 - Migração S4 - JS
    FROM zlest0039
    INTO TABLE it_zlest0039_corrigido
  WHERE dias_transito NE space.

  CHECK NOT it_zlest0039_corrigido IS INITIAL.

  LOOP AT it_zlest0039_corrigido INTO wa_zlest0039_corrigido.

    IF ( NOT wa_zlest0039_corrigido-datachegada IS INITIAL ).

      UPDATE zlest0039 SET dias_transito = 0  WHERE  datasaida  EQ wa_zlest0039-datasaida
                                              AND placa_cav     EQ wa_zlest0039-placa_cav
                                              AND vbeln         EQ wa_zlest0039-vbeln
                                              AND ebeln         EQ wa_zlest0039-ebeln.

    ENDIF.
  ENDLOOP.
ENDFORM.                    " Z_VALIDACAO_DIAS_CORRIGIDO
