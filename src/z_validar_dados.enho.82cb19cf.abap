"Name: \PR:MP966600\FO:PAGE_NUMBERS\SE:END\EI
ENHANCEMENT 0 Z_VALIDAR_DADOS.
  CASE P9666-ZTPBENEF.
    WHEN 'GR'.
      LOOP AT SCREEN.
        IF screen-name EQ '*P9666-ZVALOR'.
          screen-input = 0.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name EQ 'P9666-ZVALOR'.
          screen-input = 0.
          screen-invisible = 0.
          MODIFY SCREEN.
          clear P9666-ZVALOR.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN 'PR'.
      LOOP AT SCREEN.
        IF screen-name EQ '*P9666-ZPERC'.
          screen-input = 0.
          screen-invisible = 0.

          MODIFY SCREEN.

        ENDIF.
        IF screen-name EQ 'P9666-ZPERC'.
          screen-input = 0.
          screen-invisible = 0.
          MODIFY SCREEN.
          clear P9666-ZPERC.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN 'PM'.
      LOOP AT SCREEN.
        IF screen-name EQ '*P9666-ZPERC'.
          screen-input = 0.
          screen-invisible = 0.

          MODIFY SCREEN.

        ENDIF.
        IF screen-name EQ 'P9666-ZPERC'.
          screen-input = 0.
          screen-invisible = 0.
          MODIFY SCREEN.
          clear P9666-ZPERC.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN OTHERS.

  ENDCASE.
ENDENHANCEMENT.
