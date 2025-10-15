*&---------------------------------------------------------------------*
*& Report Z_TESTE_SAFRA_ORDER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT z_teste_safra_order.

TABLES: zsdt0082.

DATA: lc_dados   TYPE zmme_dados_safra,
      lv_mesg    TYPE string,
      lc_insumos TYPE REF TO zcl_distribuicao_insumos.

PARAMETERS: p_nrosol TYPE zsdt0082-nro_sol,
            p_seq    TYPE zsdt0082-seq.

START-OF-SELECTION.

  CREATE OBJECT lc_insumos.

  SELECT SINGLE *
    FROM zsdt0082
    INTO @DATA(w_0082)
   WHERE nro_sol = @p_nrosol
     AND seq     = @p_seq.

  CHECK sy-subrc = 0.

  lc_dados-nro_sol = w_0082-nro_sol.
  lc_dados-seq     = w_0082-seq.
  lc_dados-vbeln   = w_0082-vbeln.
  lc_dados-posnr   = w_0082-posnr.

  lc_insumos->set_integrar_safra_control( EXPORTING i_dados = lc_dados IMPORTING e_msg_erro = lv_mesg ).

  BREAK-POINT.
