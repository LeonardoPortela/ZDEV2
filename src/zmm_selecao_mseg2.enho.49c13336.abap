"Name: \PR:RM07DOCS\FO:FIELDS_FOR_SELECTION\SE:END\EI
ENHANCEMENT 0 ZMM_SELECAO_MSEG2.
*
*-CS2020000758 - 13.05.2021 - JT - inicio
  READ TABLE g_t_fields_new INTO DATA(w_fields_new) WITH KEY fieldname = 'MSEG~XBLNR_MKPF'.
  IF sy-subrc = 0.
    w_fields_new-fieldname     = 'MKPF~XBLNR'.
    MODIFY g_t_fields_new   FROM w_fields_new INDEX sy-tabix.
  ENDIF.
*-CS2020000758 - 13.05.2021 - JT - fim
*
ENDENHANCEMENT.
