"Name: \FU:J_1B_SD_CFOP\SE:BEGIN\EI
ENHANCEMENT 0 Z_1B_SD_CFOP_01.

  "Tratamento para não determinar o CFOP para os tipos de O.V abaixo.
  "O CFOP para esses tipos, será determinado na exit MV45AFZZ.
  CHECK ( I_VBAK-AUART NE 'ZCFH' ) AND
        ( I_VBAK-AUART NE 'ZCFR' ).

ENDENHANCEMENT.
