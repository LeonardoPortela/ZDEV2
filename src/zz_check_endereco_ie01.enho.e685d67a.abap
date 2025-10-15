"Name: \PR:SAPMIEQ0\FO:ADRESSE_VERBUCHEN_F10\SE:END\EI
ENHANCEMENT 0 ZZ_CHECK_ENDERECO_IE01.
**USER STORY 96100 / Anderson Oenning
"Check enderenço.

  FIELD-SYMBOLS <FS_DIADR> TYPE DIADR.

  ASSIGN ('(SAPLITO0)DIADR')   TO <FS_DIADR>.
ASSIGN ('(SAPMIEQ0)DIADR')     TO <FS_DIADR>. "// LSZA1F03


  data: ls_adrc TYPE adrc.
    IF 'IE01_IE02_IE31' CS sy-tcode.
      IF EQUI-eqtyp EQ '1'
      OR EQUI-eqtyp EQ '2'
      OR EQUI-eqtyp EQ '3'
      OR EQUI-eqtyp EQ '4'.

      IF iloa-adrnr IS INITIAL.

        MESSAGE E024(SD) WITH 'As informações de endereço, ABA LOCALIZAÇÃO'
                              'são de preenchimento obrigatório'.

        EXIT.
       ENDIF.
      ENDIF.
     ENDIF.

**USER STORY 96100 / Anderson Oenning
ENDENHANCEMENT.
