
WRITE i_data_fabricacao TO g_data_fabricacao USING EDIT MASK
       '__/__/____'.

WRITE i_data_validade   TO g_data_validade   USING EDIT MASK
       '__/__/____'.

MOVE i_numero_lote      TO g_numero_lote.

CASE i_selo.
  WHEN '01'.
*>>>Begin-Stefanini-10.06.2024 - Vitor Rienzo
*    g_selo_imagem = 'SELO_ÓLEO_DE_SOJA_DEGOMADO'.
*    g_selo_imagem = 'SELO_ÓLEO_DE_SOJA_DEGOMADO_N1'.
     g_selo_imagem = 'TESTE_JAIME1'.
*>>>End-Stefanini-10.06.2024 - Vitor Rienzo

  WHEN '02'.

    g_selo_imagem = 'SELO_FARELO_DE_SOJA_COMUM'.

  WHEN '03'.

    g_selo_imagem = 'SELO_FARELO_DE_SOJA_HIPRO'.

  WHEN '04'.

    g_selo_imagem = 'SELO_CASCA_DE_SOJA'.

ENDCASE.
