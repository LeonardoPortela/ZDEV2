"formatar data do dia
DATA VMES(10).
CASE SY-DATUM+4(2).
  WHEN '01'.
    VMES = 'Janeiro'.
  WHEN '02'.
    VMES = 'Fevereiro'.
  WHEN '03'.
    VMES = 'Março'.
  WHEN '04'.
    VMES = 'Abril'.
  WHEN '05'.
    VMES = 'Maio'.
  WHEN '06'.
    VMES = 'Junho'.
  WHEN '07'.
    VMES = 'Julho'.
  WHEN '08'.
    VMES = 'Agosto'.
  WHEN '09'.
    VMES = 'Setembro'.
  WHEN '10'.
    VMES = 'Outubro'.
  WHEN '11'.
    VMES = 'Novembro'.
  WHEN '11'.
    VMES = 'Dezembro'.
ENDCASE.

CONCATENATE SY-DATUM+6(2) 'de' VMES 'de' SY-DATUM+0(4) INTO WG_DATA SEPARATED BY SPACE.
























