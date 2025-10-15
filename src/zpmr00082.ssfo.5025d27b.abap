CASE w_t001-bukrs.
  WHEN '0048'.
    g_logo_imagem = 'LOGO_NOVO'.
  WHEN '0039'.
    g_logo_imagem = 'LOGO_UNITAPAJOS_NOVO2'.
  WHEN OTHERS.
    g_logo_imagem = 'LOGO_NOVO'.
ENDCASE.

























