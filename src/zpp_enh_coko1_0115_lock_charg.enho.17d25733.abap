"Name: \PR:SAPLCOKO1\FO:_O_SCR_MOD_CHARGE\SE:BEGIN\EI
ENHANCEMENT 0 ZPP_ENH_COKO1_0115_LOCK_CHARG.

" --- Tipos para leitura da TVARVC ---
TYPES: BEGIN OF ty_tvarvc_entry,
         werks TYPE werks_d,
         auart TYPE aufart,
       END OF ty_tvarvc_entry.

TYPES: ty_tvarvc_tab TYPE SORTED TABLE OF ty_tvarvc_entry
       WITH UNIQUE KEY werks auart.

DATA: lt_tvarvc TYPE ty_tvarvc_tab.

" --- Lê valores autorizados ---
SELECT low AS werks, high AS auart
  FROM tvarvc
  INTO TABLE @lt_tvarvc
  WHERE name = 'Z_ATRIBUI_CENTRO_TP_ORDEM'.

" --- Verifica se a combinação está na lista ---
DATA(ls_match) = FILTER #( lt_tvarvc
  WHERE werks = caufvd-werks
    AND auart = caufvd-auart ).

IF lines( ls_match ) = 0.
  RETURN. " Não autorizado → sai do enhancement
ENDIF.


*----------------------------------------------------------------------
* Daqui para baixo, só executa se estiver autorizado na TVARVC
*----------------------------------------------------------------------

" Bloqueio do campo CHARG e botão de criação
LOOP AT SCREEN.
  IF screen-name = 'AFPOD-CHARG'.
    screen-input = 0.
    MODIFY SCREEN.
  ENDIF.

  IF screen-name = 'PUSH_BATCH_CREATE'.
    screen-active = 0.
    MODIFY SCREEN.
  ENDIF.
ENDLOOP.


********* Definição do número do LOTE ********* (Chamada da função que cria o lote está na exit -->  METHOD if_ex_workorder_update~before_update.)
IF afpod-charg IS INITIAL.

DATA(lv_lote) = zcl_pp_util=>get_lote_codigo( is_caufvd = caufvd ).
afpod-charg = lv_lote.


ENDIF.
********* LOTE ********* - fim



ENDENHANCEMENT.
