*&---------------------------------------------------------------------*
*& Report  ZGL003_DRE_DADOS_DESBLOQ
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zgl003_dre_dados_desbloq MESSAGE-ID z01.

INITIALIZATION.

  DELETE FROM zgl013_espera.

  COMMIT WORK.

  MESSAGE i000 WITH 'Processamento de DRE desbloqueado!'.
