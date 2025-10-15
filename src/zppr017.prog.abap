*&---------------------------------------------------------------------*
*& Report  ZPPR017
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zppr017.

TABLES: zpp_kuhlmann_hvi.

DATA: it_hvis TYPE TABLE OF zpp_kuhlmann_hvi.
DATA:t_servico            TYPE STANDARD TABLE OF  rgsb4.
CLEAR: t_servico.

SELECTION-SCREEN BEGIN OF BLOCK int01 WITH FRAME TITLE text-001.
SELECT-OPTIONS: sfardo FOR zpp_kuhlmann_hvi-fardo,
                snrose FOR zpp_kuhlmann_hvi-os_nr,"NO INTERVALS NO-EXTENSION,
                snrrom FOR zpp_kuhlmann_hvi-romaneio,"NO INTERVALS NO-EXTENSION,
                sdtent FOR zpp_kuhlmann_hvi-data_entrada NO-EXTENSION,
                sdtana FOR zpp_kuhlmann_hvi-data_analise NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK int01.

SELECTION-SCREEN BEGIN OF BLOCK int02 WITH FRAME TITLE text-002.
PARAMETERS: ckbuscar AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK int02.

INITIALIZATION.

  IF sy-batch EQ abap_true.

    TRY .
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd  = DATA(e_qtd) ).
      CATCH zcx_job.
    ENDTRY.

    IF e_qtd GT 1.
      LEAVE PROGRAM.
    ENDIF.

*    SELECT * INTO TABLE @DATA(IT_CLIENTES)
*      FROM ZTSAFRAFARDOS.
*
*    DELETE IT_CLIENTES WHERE LOGIN EQ SPACE.
*    DELETE IT_CLIENTES WHERE SENHA EQ SPACE.
*    SORT IT_CLIENTES BY LOGIN.
*    DELETE ADJACENT DUPLICATES FROM IT_CLIENTES COMPARING LOGIN.

*    LOOP AT IT_CLIENTES INTO DATA(WA_CLIENTES).

    "Verificar tipo de acesso.
    SELECT SINGLE *
    FROM setleaf
    INTO @DATA(i_data)
    WHERE setname EQ 'MAGI_PP_KUHLMANN'.

    IF i_data IS NOT INITIAL.

      CALL FUNCTION 'G_SET_GET_ALL_VALUES'
        EXPORTING
          class           = '0000'
          setnr           = 'MAGI_PP_KUHLMANN'
          no_descriptions = ' '
        TABLES
          set_values      = t_servico
        EXCEPTIONS
          set_not_found   = 1
          OTHERS          = 2.
    ENDIF.

    IF t_servico IS NOT INITIAL.
      LOOP AT t_servico ASSIGNING FIELD-SYMBOL(<ws_servico>).

        TRY .

            zcl_integracao_hvi_kuhlmann=>zif_integracao_hvi_kuhlmann~get_instance( i_servico = CONV #( <ws_servico>-from )
              )->set_hvi_buscar(
              ).

          CATCH zcx_integracao INTO DATA(ex_integra).    "
            ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          CATCH zcx_error INTO DATA(ex_error).    "  "
            ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        ENDTRY.
      ENDLOOP.
    ENDIF.
*    ENDLOOP.

    LEAVE PROGRAM.

  ENDIF.

START-OF-SELECTION.

  IF ckbuscar EQ abap_true.
*
*    SELECT * INTO TABLE @IT_CLIENTES
*      FROM ZTSAFRAFARDOS.
*
*    DELETE IT_CLIENTES WHERE LOGIN EQ SPACE.
*    DELETE IT_CLIENTES WHERE SENHA EQ SPACE.
*    SORT IT_CLIENTES BY LOGIN.
*    DELETE ADJACENT DUPLICATES FROM IT_CLIENTES COMPARING LOGIN.

    DATA: i_entrada_inicio TYPE string,
          i_entrada_final	 TYPE string,
          i_analise_inicio TYPE string,
          i_analise_final	 TYPE string.

    i_entrada_inicio = COND #( LET clet = sdtent-low IN WHEN clet IS INITIAL THEN space ELSE |{ sdtent-low+6(2) }/{ sdtent-low+4(2) }/{ sdtent-low(4) }| ).
    i_entrada_final  = COND #( LET clet = sdtent-high IN WHEN clet IS INITIAL THEN space ELSE |{ sdtent-high+6(2) }/{ sdtent-high+4(2) }/{ sdtent-high(4) }| ).
    i_analise_inicio = COND #( LET clet = sdtana-low IN WHEN clet IS INITIAL THEN space ELSE |{ sdtana-low+6(2) }/{ sdtana-low+4(2) }/{ sdtana-low(4) }| ).
    i_analise_final  = COND #( LET clet = sdtana-high IN WHEN clet IS INITIAL THEN space ELSE |{ sdtana-high+6(2) }/{ sdtana-high+4(2) }/{ sdtana-high(4) }| ).

*    LOOP AT IT_CLIENTES INTO WA_CLIENTES.

    "Verificar tipo de acesso.
    CLEAR: i_data.
    SELECT SINGLE *
    FROM setleaf
    INTO i_data
    WHERE setname EQ 'MAGI_PP_KUHLMANN'.

    IF i_data IS NOT INITIAL.

      CALL FUNCTION 'G_SET_GET_ALL_VALUES'
        EXPORTING
          class           = '0000'
          setnr           = 'MAGI_PP_KUHLMANN'
          no_descriptions = ' '
        TABLES
          set_values      = t_servico
        EXCEPTIONS
          set_not_found   = 1
          OTHERS          = 2.
    ENDIF.

    IF t_servico IS NOT INITIAL.

      LOOP AT t_servico ASSIGNING <ws_servico>.
        TRY .

            zcl_integracao_cli_kuhlmann=>zif_integracao_cli_kuhlmann~get_instance( i_servico =  CONV #( <ws_servico>-from )
            )->set_servico( i_servico =  CONV #( <ws_servico>-from ) ).

            zcl_integracao_cli_kuhlmann=>zif_integracao_cli_kuhlmann~get_instance( i_servico =  CONV #( <ws_servico>-from )
            )->set_clientes_buscar( IMPORTING e_clientes = DATA(e_clientes) ).

            LOOP AT e_clientes INTO DATA(wa_clientes).

              zcl_integracao_rec_kuhlmann=>zif_integracao_rec_kuhlmann~get_instance( i_servico = CONV #( <ws_servico>-from )
                )->set_servico( i_servico =  CONV #( <ws_servico>-from ) ).

              zcl_integracao_rec_kuhlmann=>zif_integracao_rec_kuhlmann~get_instance( i_servico = CONV #( <ws_servico>-from )
              )->set_hvi_recuperar(
              EXPORTING
                i_cliente_id                 = wa_clientes-id
                i_numero_os                  = CONV #( snrose-low )
                i_romaneio                   = CONV #( snrrom-low )
                i_entrada_inicio             = i_entrada_inicio
                i_entrada_final              = i_entrada_final
                i_analise_inicio             = i_analise_inicio
                i_analise_final              = i_analise_final
                "I_USUARIO                    = CONV #( WA_CLIENTES-LOGIN )
                "I_SENHA                      = CONV #( WA_CLIENTES-SENHA )
            ).

            ENDLOOP.


          CATCH zcx_integracao INTO ex_integra.    "
            ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          CATCH zcx_error INTO ex_error.    "  "
            ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        ENDTRY.
      ENDLOOP.
    ENDIF.
*    ENDLOOP.

  ENDIF.

  SELECT * INTO TABLE @it_hvis
    FROM zpp_kuhlmann_hvi
   WHERE fardo         IN @sfardo
     AND os_nr         IN @snrose
     AND romaneio      IN @snrrom
     AND data_entrada  IN @sdtent
     AND data_analise  IN @sdtana.

END-OF-SELECTION.

  CALL SCREEN 0100.

  INCLUDE zppr017_status_0100o01.
