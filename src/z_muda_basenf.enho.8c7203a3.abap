"Name: \PR:SAPLJ1BF\FO:TAX_VALUES_DEFAULT\SE:END\EI
ENHANCEMENT 0 Z_MUDA_BASENF.
*REGRA DA J1BTAX
  IF f_itmtyp = '02'. "transferencia
      LOOP AT F_TAX_DATA.
          IF F_TAX_DATA-TAXTYP = 'ICM3'.
              IF wa_t007a-J_1BTXICEX = 'X'. "BASE EXCLUIDA
                 if  F_TAX_DATA-EXCBAS = 0.
                     F_TAX_DATA-EXCBAS =  F_TAX_DATA-OTHBAS.
                     F_TAX_DATA-OTHBAS = 0.
                     MODIFY F_TAX_DATA.
                 endif.
              else.
                 if  F_TAX_DATA-OTHBAS = 0.
                     F_TAX_DATA-OTHBAS =  F_TAX_DATA-EXCBAS.
                     F_TAX_DATA-EXCBAS = 0.
                     MODIFY F_TAX_DATA.
                 endif.
              ENDIF.
          ENDIF.
           IF F_TAX_DATA-TAXTYP = 'IPI3'.
              IF wa_t007a-J_1BTXIPEX = 'X'. "BASE EXCLUIDA
                 if  F_TAX_DATA-EXCBAS = 0.
                     F_TAX_DATA-EXCBAS =  F_TAX_DATA-OTHBAS.
                     F_TAX_DATA-OTHBAS = 0.
                     MODIFY F_TAX_DATA.
                 endif.
              else.
                 if  F_TAX_DATA-OTHBAS = 0.
                     F_TAX_DATA-OTHBAS =  F_TAX_DATA-EXCBAS.
                     F_TAX_DATA-EXCBAS = 0.
                     MODIFY F_TAX_DATA.
                 endif.
              ENDIF.
          ENDIF.
      ENDLOOP.
  ENDIF.
ENDENHANCEMENT.
