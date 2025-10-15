*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 15/12/2010                                              &*
*& Descrição: Criação de Nota fiscal. Execução via JOB                &*
*& Transação: ZNFJ0002                                                &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&--------------------------------------------------------------------&*
REPORT  zwrj0002.

*&---------------------------------------------------------------------*
*&      types
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_doc,
         docnum TYPE j_1bnfdoc-docnum,
         nfenum TYPE j_1bnfdoc-nfenum,
         series TYPE j_1bnfdoc-series,
       END OF ty_doc.

TYPES: BEGIN OF ty_likp,
         vbeln TYPE likp-vbeln,
         lifex TYPE likp-vbeln,
       END OF ty_likp.


*&---------------------------------------------------------------------*
*&      Tabelas internas
*&---------------------------------------------------------------------*
DATA: tg_0008 TYPE TABLE OF zfiwrt0008 WITH HEADER LINE,
      tg_0009 TYPE TABLE OF zfiwrt0009 WITH HEADER LINE,
      tg_doc  TYPE TABLE OF ty_doc WITH HEADER LINE,
      tg_likp TYPE TABLE OF ty_likp WITH HEADER LINE.

*&---------------------------------------------------------------------*
*&      Variaveis
*&---------------------------------------------------------------------*



*&---------------------------------------------------------------------*
*&      Start-Of-Selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: vg_job      TYPE i.

  SELECT SINGLE COUNT( * ) INTO vg_job
    FROM tbtco
   WHERE jobname EQ 'ZNFW0002_CARIMBA_NF'
     AND status EQ 'R'.

  IF ( vg_job EQ 1 ).
    PERFORM limpa_estruturas.
    PERFORM seleciona_dados.
    PERFORM executa_bapi.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados .
  SELECT *
    FROM zfiwrt0008
    INTO TABLE tg_0008
     WHERE budat = sy-datum.

  SORT tg_0008 BY ebeln.
  DELETE tg_0008 WHERE ebeln IS INITIAL.

  SORT tg_0008 BY docnum.
  DELETE tg_0008 WHERE docnum IS INITIAL.

  IF tg_0008[] IS NOT INITIAL.
    SELECT docnum
           nfenum
           series INTO TABLE tg_doc
      FROM j_1bnfdoc
       FOR ALL ENTRIES IN tg_0008
        WHERE docnum EQ tg_0008-docnum.

    SORT tg_doc BY docnum.

    SELECT *
      FROM zfiwrt0009
      INTO TABLE tg_0009
       FOR ALL ENTRIES IN tg_0008
        WHERE seq_lcto EQ tg_0008-seq_lcto.

    SORT tg_0009 BY vbeln_r.
    DELETE tg_0009 WHERE vbeln_r IS INITIAL.

    IF tg_0009[] IS NOT INITIAL.
      SELECT vbeln
             lifex INTO TABLE tg_likp
        FROM likp
        FOR ALL ENTRIES IN tg_0009
        WHERE vbeln = tg_0009-vbeln_r.

      SORT tg_likp BY lifex.
      LOOP AT tg_likp.
        CONDENSE tg_likp-lifex NO-GAPS.
        DATA(_tam) = strlen( tg_likp-lifex ).
        IF _tam LE 2.
          CLEAR tg_likp-lifex.
          MODIFY tg_likp INDEX sy-tabix TRANSPORTING lifex.
        ENDIF.
      ENDLOOP.
      DELETE tg_likp WHERE lifex IS NOT INITIAL.
    ENDIF.

    SORT tg_0008 BY seq_lcto.
  ENDIF.
ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  EXECUTA_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM executa_bapi .

  DATA: lv_ablad_iv LIKE  vbpa-ablad,
        lv_lifex_iv LIKE  likp-lifex,
        it_xekes    TYPE STANDARD TABLE OF uekes,
        wa_xekes    LIKE LINE OF it_xekes.

  WRITE: 'Log Registros Processados'.
  WRITE: /.
  WRITE: 'VBELN;LIFEX'.
  IF tg_likp[] IS NOT INITIAL.
    LOOP AT tg_likp .

      CLEAR: lv_ablad_iv,
             lv_lifex_iv.

      READ TABLE tg_0009 INTO tg_0009 WITH KEY vbeln_r = tg_likp-vbeln BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE tg_0008 INTO tg_0008 WITH KEY seq_lcto = tg_0009-seq_lcto BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE tg_doc INTO tg_doc WITH KEY docnum = tg_0008-docnum BINARY SEARCH.
          IF sy-subrc = 0.
            SHIFT tg_doc-nfenum LEFT DELETING LEADING '0'.
            CONDENSE tg_doc-nfenum NO-GAPS.
            lv_lifex_iv = tg_doc-nfenum && '-' && tg_doc-series.

            CALL FUNCTION 'JIT09_UPDATE_ABLAD_IN_DELIVERY'
              EXPORTING
                vbeln_iv  = tg_likp-vbeln
                ablad_iv  = lv_ablad_iv
                lifex_iv  = lv_lifex_iv
              EXCEPTIONS
                no_update = 1
                OTHERS    = 2.

            IF sy-subrc <> 0.
              WRITE: tg_likp-vbeln && ';ERRO AO ATUALIZAR CAMPO LIFEX'.
            ELSE.
              REFRESH it_xekes.
              SELECT *
                FROM ekes
                INTO CORRESPONDING FIELDS OF TABLE it_xekes
                WHERE ebeln  = tg_0008-ebeln
                AND   vbeln  = tg_likp-vbeln.
              IF it_xekes[] IS NOT INITIAL.
                LOOP AT it_xekes INTO wa_xekes.
                  wa_xekes-xblnr = lv_lifex_iv.
                  wa_xekes-kz    = 'U'.
                  MODIFY it_xekes FROM wa_xekes INDEX sy-tabix TRANSPORTING xblnr kz.
                ENDLOOP.
                CALL FUNCTION 'ME_CONFIRMATION_UPDATE'
                  EXPORTING
                    i_ebeln = tg_likp-vbeln
                  TABLES
                    xekes   = it_xekes.
              ENDIF.
              WRITE: tg_likp-vbeln && ';' && tg_likp-lifex.
              COMMIT WORK AND WAIT.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSE.
    WRITE: 'NENHUM REGISTRO ENCONTRADO'.
  ENDIF.
ENDFORM.                    " EXECUTA_BAPI
*&---------------------------------------------------------------------*
*&      Form  LIMPA_ESTRUTURAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpa_estruturas .
  CLEAR: tg_0008,
         tg_0009,
         tg_doc.

  REFRESH: tg_0008,
           tg_0009,
           tg_doc.
ENDFORM.                    " LIMPA_ESTRUTURAS
