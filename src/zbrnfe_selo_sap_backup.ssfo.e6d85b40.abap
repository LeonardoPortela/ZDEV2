
WRITE i_data_fabricacao TO g_data_fabricacao USING EDIT MASK
       '__/__/____'.

WRITE i_data_validade   TO g_data_validade   USING EDIT MASK
       '__/__/____'.

MOVE i_numero_lote      TO g_numero_lote.

CASE i_selo.
  WHEN '01'.
    g_selo_imagem = 'SELO_SAP_OLEO_SOJA_DEGOMADO'.

  WHEN '02'.
    g_selo_imagem = 'SELO_SAP_FARELO_SOJA_COMUM'.

  WHEN '03'.
    g_selo_imagem = 'SELO_SAP_FARELO_SOJA_HIPRO'.

  WHEN '04'.
    g_selo_imagem = 'SELO_SAP_CASCA_SOJA'.

ENDCASE.




















