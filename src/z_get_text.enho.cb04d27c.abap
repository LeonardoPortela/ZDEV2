"Name: \PR:RM06BB30\FO:BUILD_OBJECT\SE:END\EI
ENHANCEMENT 0 Z_GET_TEXT.
*Insere texto da requisicao de compras na PO.
 TYPES: BEGIN OF ty_ekpo,
          ebeln TYPE ekpo-ebeln,
          ebelp TYPE ekpo-ebelp,
          banfn TYPE ekpo-banfn,
          bnfpo TYPE ekpo-bnfpo,
        END OF ty_ekpo,

        BEGIN OF ty_eban,
          banfn TYPE eban-banfn,
          bnfpo TYPE eban-bnfpo,
        END OF ty_eban.

 DATA: tl_ekpo      TYPE TABLE OF ty_ekpo WITH HEADER LINE,
       tl_eban      TYPE TABLE OF ty_eban WITH HEADER LINE,
       tl_lines     TYPE TABLE OF tline WITH HEADER LINE,
       tl_lines_aux TYPE TABLE OF tline WITH HEADER LINE,
       wl_ebeln     TYPE bapimepoheader-po_number,
       wl_name      TYPE thead-tdname,
       tl_return    TYPE TABLE OF bapiret2 WITH HEADER LINE,
       tl_text      TYPE TABLE OF bapimepotextheader WITH HEADER LINE.

 REFRESH: tl_ekpo, tl_eban, tl_lines, tl_lines_aux, tl_text, tl_return.
 CLEAR: wl_ebeln, wl_name.

 IF ( sy-tcode EQ 'ME59N'
 OR sy-cprog EQ 'RM06BB30' )
 AND ch_todo-ebeln IS NOT INITIAL.
**  É NESCESSARIO A ESPERAR 1 SEGUNDO PARA O NUMERO DO DOCUMENTO TER SIDO ATUALIZADO NA TABELA DA PO.
   WHILE tl_ekpo[] IS INITIAL.
     WAIT UP TO 1 SECONDS.
     SELECT ebeln ebelp banfn bnfpo
       FROM ekpo
       INTO TABLE tl_ekpo
        WHERE ebeln EQ ch_todo-ebeln.

   ENDWHILE.
   IF sy-subrc IS INITIAL.
     SELECT banfn bnfpo
       FROM eban
       INTO TABLE tl_eban
        FOR ALL ENTRIES IN tl_ekpo
        WHERE banfn EQ tl_ekpo-banfn
          AND bnfpo EQ tl_ekpo-bnfpo.


     LOOP AT tl_eban.
       wl_name = tl_eban-banfn.
       CALL FUNCTION 'READ_TEXT'
         EXPORTING
           client                  = sy-mandt
           id                      = 'B01'
           language                = 'P'
           name                    = wl_name
           object                  = 'EBANH'
         TABLES
           lines                   = tl_lines_aux
         EXCEPTIONS
           id                      = 1
           language                = 2
           name                    = 3
           not_found               = 4
           object                  = 5
           reference_check         = 6
           wrong_access_to_archive = 7
           OTHERS                  = 8.

       APPEND LINES OF tl_lines_aux TO tl_lines.
       DELETE tl_eban WHERE banfn EQ tl_eban-banfn.
     ENDLOOP.
     IF tl_lines[] IS NOT INITIAL.
       wl_ebeln = ch_todo-ebeln.

       LOOP AT tl_lines.
         MOVE: wl_ebeln        TO tl_text-po_number,
               'F00'           TO tl_text-text_id,
               tl_lines-tdformat TO tl_text-text_form,
               tl_lines-tdline TO tl_text-text_line.

         APPEND tl_text.
         CLEAR: tl_text.
       ENDLOOP.

       CALL FUNCTION 'BAPI_PO_CHANGE' "#EC CI_USAGE_OK[2438131]
         EXPORTING
           purchaseorder = wl_ebeln
         TABLES
           return        = tl_return
           potextheader  = tl_text.

       CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
           wait = 'X'.

     ENDIF.
   ELSE.

   ENDIF.
 ELSE.

 ENDIF.
ENDENHANCEMENT.
