*&---------------------------------------------------------------------*
*& Report ZMMR0208
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmmr0208.
TABLES: zib_cte_dist_ter.

SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE TEXT-c01 .
  SELECT-OPTIONS: p_key FOR zib_cte_dist_ter-cd_chave_cte NO INTERVALS.
SELECTION-SCREEN END OF BLOCK part1.

DATA: obj_cte TYPE REF TO zcl_cte_dist_g.

IF p_key IS NOT INITIAL.

  LOOP AT p_key ASSIGNING FIELD-SYMBOL(<_line>).

    FREE: obj_cte.

    CREATE OBJECT obj_cte.

    CALL METHOD obj_cte->gerar_doc_transporte
      EXPORTING
        p_cte_chave = <_line>-low
      EXCEPTIONS
        doc_transp  = 1
        OTHERS      = 2.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDLOOP.

ENDIF.
