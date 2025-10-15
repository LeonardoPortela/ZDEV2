"Name: \PR:ZYVALIDATER004\FO:F_IMPLEMENTACAO_001\SE:BEGIN\EI
ENHANCEMENT 0 ZZ_CHECK_DESC_ENDERECO_IE01.
 "USER STORY 96100 / Obrigatoriedade em Endereço equipamento / Anderson Oenning
 IF 'IE01_IE02_IE31' CS sy-tcode.
  IF addr1_data-name1 is INITIAL.
  MESSAGE e024(sd) WITH 'É obrigatorio o preenchimento do '
                         'nome do endereço'.
   exit.
  ENDIF.
 endif.
ENDENHANCEMENT.
