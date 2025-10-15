"Name: \PR:RFFOBR_D\FO:FILL_DETAILS_FEBRABAN\SE:END\EI
ENHANCEMENT 0 Z_UPDATE_P_Q.
*

  IF j_1bdmexp-p01 EQ '001'."Banco do Brasil
    j_1bdmexp-p07 = '01'.         "Código do Movimento
    j_1bdmexp-p14 = '7'.          "Código da Carteira
    j_1bdmexp-p15 = '1'.          "Forma de Cadastramto Título
    j_1bdmexp-p17 = '2'.          "Identific. de emis.Boleto
    j_1bdmexp-p18 = '2'.          "Identific. da Distribuição
    j_1bdmexp-p22 = '00000'.      "Agência encarregada cobrança
    j_1bdmexp-p23 = ' '.          "Dígito verificador agência
    j_1bdmexp-p35 = '3'.          "Código para protesto

    IF regup-belnr IS NOT INITIAL.
      j_1bdmexp-p19 = regup-xref2. "Nro documento cobrança
    ELSE.
      CLEAR j_1bdmexp-p19.
    ENDIF.
  ENDIF.

  IF j_1bdmexq-q01 EQ '001'."Banco do Brasil
    IF regup-zlsch EQ 'D'.
      j_1bdmexq-q07 = '01'.
    ENDIF.
  ENDIF.
ENDENHANCEMENT.
