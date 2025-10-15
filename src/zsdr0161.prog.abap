*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Jaime Tassoni                                           &*
*& Data.....: 08.07.2022                                              &*
*& Descrição: Processamento Transferencia FArdos - Trace Cotton       &*
*&--------------------------------------------------------------------&*
*& Projeto  : Algodão                                                 &*
*&--------------------------------------------------------------------&*
REPORT zsdr0161 MESSAGE-ID zjob.

************************************************************************
* tabelas
************************************************************************
TABLES: zsdt0330.

************************************************************************
*  parametro ID_CARGA
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : s_carga  FOR zsdt0330-id_carga.
  PARAMETERS     : p_st_far LIKE zsdt0330-status_fardo,
                   p_st_lot LIKE zsdt0330-status_gera_lote.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-001.
  PARAMETERS     : p_nv_far LIKE zsdt0330-status_fardo,
                   p_nv_lot LIKE zsdt0330-status_gera_lote.
SELECTION-SCREEN END OF BLOCK b2.

************************************************************************
*& types
************************************************************************
TYPES: BEGIN OF ty_proc,
         chave_referencia TYPE zde_id_referencia.
TYPES: END   OF ty_proc.

************************************************************************
*& variaveis globais
************************************************************************
DATA: t_zsdt0330 TYPE TABLE OF zsdt0330,
      w_zsdt0330 TYPE zsdt0330.

************************************************************************
*&      Start-Of-Selection
************************************************************************
START-OF-SELECTION.

  PERFORM f_processa_dados.

************************************************************************
*&-processamento dos fardos
************************************************************************
FORM f_processa_dados.

*------------------------------------------
*- recupera ch_referencia
*------------------------------------------
  SELECT *
    FROM zsdt0330
    INTO TABLE t_zsdt0330
   WHERE id_carga        IN s_carga
     AND status_fardo     = p_st_far
     AND status_gera_lote = p_st_lot
     AND cancelado        = abap_false.

  LOOP AT t_zsdt0330 INTO w_zsdt0330.
    IF p_nv_far IS NOT INITIAL.
      w_zsdt0330-status_fardo     = p_nv_far.
    ENDIF.
    IF p_nv_lot IS NOT INITIAL.
      w_zsdt0330-status_gera_lote = p_nv_lot.
    ENDIF.

    "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>
    "MODIFY zsdt0330 FROM w_zsdt0330.
    UPDATE zsdt0330 SET status_gera_lote  = w_zsdt0330-status_gera_lote
                        status_fardo      = w_zsdt0330-status_fardo
                        WHERE id_carga      EQ w_zsdt0330-id_carga
                          AND matnr         EQ w_zsdt0330-matnr
                          AND werks         EQ w_zsdt0330-werks
                          AND lgort         EQ w_zsdt0330-lgort
                          AND acharg        EQ w_zsdt0330-acharg
                          AND safra         EQ w_zsdt0330-safra
                          AND seq           EQ w_zsdt0330-seq.

    "SD - Ganho Peso Automatico Algodao US #145369 - WPP <<---

  ENDLOOP.

  COMMIT WORK.

ENDFORM.

************************************************************************
************************************************************************
