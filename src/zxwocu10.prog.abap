*&---------------------------------------------------------------------*
*&  Include           ZXWOCU10
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_TQ80) LIKE  TQ80 STRUCTURE  TQ80
*"     VALUE(I_VRGNG) LIKE  TJ01-VRGNG OPTIONAL
*"     VALUE(I_AKTYP) LIKE  T365-AKTYP
*"     VALUE(I_VIQMEL) LIKE  VIQMEL STRUCTURE  VIQMEL
*"     VALUE(I_OBJNR) LIKE  VIQMEL-OBJNR OPTIONAL
*"     VALUE(I_UMODE) TYPE  C OPTIONAL
*"  TABLES
*"      T_VIQMFE STRUCTURE  WQMFE OPTIONAL
*"      T_VIQMUR STRUCTURE  WQMUR OPTIONAL
*"      T_VIQMMA STRUCTURE  WQMMA OPTIONAL
*"      T_VIQMSM STRUCTURE  WQMSM OPTIONAL
*"  EXCEPTIONS
*"      NO_STATUS_CHANGE
*"----------------------------------------------------------------------
*
*IF sy-tcode = 'IW32'.
*  TYPES: BEGIN OF ty_ekpo,
*           erekz TYPE ekpo-erekz,
*           elikz TYPE ekpo-elikz,
*           banfn TYPE ekpo-banfn,
*           ebeln TYPE ekpo-ebeln,
*         END OF ty_ekpo,
*
*         BEGIN OF ty_eban,
*            frgzu TYPE eban-frgzu,
*            banfn TYPE eban-banfn,
*         END OF ty_eban.
*
*  DATA: tl_ekpo TYPE TABLE OF ty_ekpo,
*        wl_ekpo TYPE ty_ekpo,
*
*        tl_eban TYPE TABLE OF ty_eban,
*        wl_eban TYPE ty_eban,
*
*        tl_ebkn TYPE TABLE OF ebkn WITH HEADER LINE,
*        lv_sepa TYPE c LENGTH 1,
*        lv_pedi TYPE bapi_msg,
*        lv_requ TYPE bapi_msg,
*        lv_msg  TYPE bapi_msg.
*
*  CASE sy-ucomm.
*    WHEN 'WTER'. "Regra para encerramento da ordem
*      SELECT *
*        FROM ebkn
*        INTO TABLE tl_ebkn
*        WHERE aufnr = i_viqmel-aufnr.
*
*      IF sy-subrc IS INITIAL.
*        SELECT *
*          FROM eban
*          INTO CORRESPONDING FIELDS OF TABLE tl_eban
*          FOR ALL ENTRIES IN tl_ebkn
*          WHERE banfn = tl_ebkn-banfn.
*
*        IF sy-subrc IS INITIAL.
*          CLEAR: lv_requ, lv_pedi, lv_sepa.
*
*          LOOP AT tl_eban INTO wl_eban WHERE frgzu = 'X'.
*            SELECT DISTINCT *
*              FROM ekpo
*              INTO CORRESPONDING FIELDS OF TABLE tl_ekpo
*              WHERE banfn = wl_eban-banfn.
*
*            IF sy-subrc IS INITIAL.
*              CLEAR: lv_sepa, lv_requ.
*
*              LOOP AT tl_ekpo INTO wl_ekpo WHERE erekz = ''
*                                             AND elikz = ''.
*                IF lv_requ IS NOT INITIAL.
*                  lv_sepa = ','.
*                ENDIF.
*                CONCATENATE lv_requ lv_sepa wl_eban-banfn INTO lv_requ.
*                CONCATENATE lv_pedi lv_sepa wl_ekpo-ebeln INTO lv_pedi.
*              ENDLOOP.
*            ELSE.
*              IF lv_requ IS NOT INITIAL.
*                lv_sepa = ','.
*              ENDIF.
*              CONCATENATE lv_requ lv_sepa wl_eban-banfn INTO lv_requ.
*            ENDIF.
*          ENDLOOP.
*        ENDIF.
*      ENDIF.
*
*      IF lv_requ IS NOT INITIAL
*      AND lv_pedi IS NOT INITIAL.
*        CONCATENATE 'Existem requisição(ões) aprovada(s) (' lv_requ '), pedido(s) (' lv_pedi ') em aberto ou aguardando finalização.' INTO lv_msg.
*        MESSAGE lv_msg TYPE 'I'.
*        MESSAGE e398(00) WITH 'Não foi possível encerrar a ordem.'.
*      ELSEIF lv_requ IS NOT INITIAL
*      AND lv_pedi IS INITIAL.
*        CONCATENATE 'Existem requisição(ões) aprovada(s) (' lv_requ ').' INTO lv_msg.
*        MESSAGE lv_msg TYPE 'I'.
*        MESSAGE e398(00) WITH 'Não foi possível encerrar a ordem.'.
*      ENDIF.
*
*  ENDCASE.
*ENDIF.
