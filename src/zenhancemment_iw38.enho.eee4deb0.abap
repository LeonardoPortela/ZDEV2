"Name: \PR:RIAUFK20\EX:EHP603_RIAUFK20_O7\EN:IUID_APPL_01_RIAUFK20\SE:BEGIN\EI
ENHANCEMENT 0 ZENHANCEMMENT_IW38.
**Inicio USER STORY 96088* / Anderson Oenning

**Passar valor para estrutura da ALV IW38.
*  LOOP AT object_tab ASSIGNING FIELD-SYMBOL(<WS_OB_TAB>).
*  CALL FUNCTION 'ZPM_CAL_DATE_PLAN'
*    EXPORTING
*      i_date_inicio       = <WS_OB_TAB>-STRMN
*      i_date_fim          = <WS_OB_TAB>-STRMN
*   IMPORTING
*     E_STATUS            = <WS_OB_TAB>-zzstatus.
*  ENDLOOP.
**Fim USER STORY 96088* / Anderson Oenning
ENDENHANCEMENT.
