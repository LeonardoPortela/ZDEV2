class ZCL_ZONA_TRANSPORTE definition
  public
  final
  create public .

public section.

  types:
    " Definição do tipo de retorno estruturado
    BEGIN OF ty_msg,
             type TYPE symsgty,   " Tipo da mensagem (S, E, W, I)
             msg1 TYPE string,    " Primeira parte da mensagem
             msg2 TYPE string,    " Segunda parte da mensagem (opcional)
           END OF ty_msg .
  types:
    " Definição da tabela interna de mensagens
    ty_t_msg TYPE STANDARD TABLE OF ty_msg WITH EMPTY KEY .

  methods CRIAR_ZONA
    importing
      !I_PAIS type LAND1
      !I_COD_ZONA type LZONE
      !I_DESC type BEZEI20
      !I_ZLATITUDE type ZDE_LATITUDE
      !I_ZLONGITUDE type ZDE_LONGITUDE
      !I_Z_URL_LOCALIZACAO type ZDE_URL_LOC
    returning
      value(E_MSG) type TY_T_MSG .   " Retorna uma tabela de mensagens
  methods ATUALIZA_ZONA
    importing
      !I_TZONE type TZONE
    returning
      value(E_MSG) type TY_T_MSG .
  methods ENQUEUE_TZONE
    importing
      !I_EXEC type CHAR1
    returning
      value(E_MSG) type TY_T_MSG .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_ZONA_TRANSPORTE IMPLEMENTATION.


  METHOD criar_zona.
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 03/04/2025|DEVK9A1XAW |NSEGATIN       | ZSDT0113 - Ajuste no cadastro de Roteiro. Chamado 171563      *
*--------------------------------------------------------------------------------------------------------*

    DATA: ls_tzone TYPE tzone,
          ls_tzont TYPE tzont,
          ls_msg   TYPE ty_msg.  " Estrutura para armazenar a mensagem

    CLEAR e_msg.
**<<<------"171563 - NMS - INI------>>>
* ENQUEUE/DEQUEUE Tabela TZONE
    e_msg = enqueue_tzone( i_exec = abap_on ).
    CHECK e_msg IS INITIAL.
**<<<------"171563 - NMS - FIM------>>>
    TRY.
        " Preencher os dados para inserção na TZONE
        ls_tzone-land1 = i_pais.
        ls_tzone-zone1 = i_cod_zona.
**<<<------"171563 - NMS - INI------>>>
        ls_tzone-zlatitude         = i_zlatitude.
        ls_tzone-zlongitude        = i_zlongitude.
        ls_tzone-z_url_localizacao = i_z_url_localizacao.
**<<<------"171563 - NMS - FIM------>>>
        INSERT INTO tzone VALUES ls_tzone.

        " Criar mensagem de sucesso
        ls_msg-type = 'S'.
        ls_msg-msg1 = 'Registro inserido com sucesso na TZONE!'.
        APPEND ls_msg TO e_msg.

      CATCH cx_sy_sql_error INTO DATA(lx_sql).
        " Criar mensagem de erro
        ls_msg-type = 'E'.
        ls_msg-msg1 = 'Erro ao inserir na TZONE'.
        ls_msg-msg2 = lx_sql->get_text( ).
        APPEND ls_msg TO e_msg.
**<<<------"171563 - NMS - INI------>>>
* ENQUEUE/DEQUEUE Tabela TZONE
        e_msg = enqueue_tzone( i_exec = abap_off ).
**<<<------"171563 - NMS - FIM------>>>
        RETURN. " Retorna imediatamente em caso de erro
    ENDTRY.

    TRY.
        " Preencher os dados para inserção na TZONT
        ls_tzont-spras  = 'P'.
        ls_tzont-land1  = i_pais.
        ls_tzont-zone1  = i_cod_zona.
        ls_tzont-vtext  = i_desc.
        INSERT INTO tzont VALUES ls_tzont.

        " Criar mensagem de sucesso
        ls_msg-type = 'S'.
        ls_msg-msg1 = 'Registro inserido com sucesso na TZONT!'.
        APPEND ls_msg TO e_msg.

      CATCH cx_sy_sql_error INTO lx_sql.
        " Criar mensagem de erro
        ls_msg-type = 'E'.
        ls_msg-msg1 = 'Erro ao inserir na TZONT'.
        ls_msg-msg2 = lx_sql->get_text( ).
**<<<------"171563 - NMS - INI------>>>
* ENQUEUE/DEQUEUE Tabela TZONE
        e_msg = enqueue_tzone( i_exec = abap_off ).
**<<<------"171563 - NMS - FIM------>>>
        APPEND ls_msg TO e_msg.
    ENDTRY.
**<<<------"171563 - NMS - INI------>>>
* ENQUEUE/DEQUEUE Tabela TZONE
    e_msg = enqueue_tzone( i_exec = abap_off ).
**<<<------"171563 - NMS - FIM------>>>
  ENDMETHOD.


  METHOD atualiza_zona.
*&-------------------------------------------------------------------------------------------------------*
*& Método         : ATUALIZA_ZONA                                                                        *
*& Chamado        : USER STORY 171563                                                                    *
*& Data           : 03/04/2025                                                                           *
*& Especificado   : Paulo Quevedo                                                                        *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 03/04/2025|DEVK9A1XAW |NSEGATIN       | ZSDT0113 - Ajuste no cadastro de Roteiro.                     *
*--------------------------------------------------------------------------------------------------------*
    DATA: ls_msg TYPE ty_msg.  " Estrutura para armazenar a mensagem

    CLEAR e_msg.
* ENQUEUE/DEQUEUE Tabela TZONE
    e_msg = enqueue_tzone( i_exec = abap_on ).
    CHECK e_msg IS INITIAL.
    TRY.
* Preencher os dados para atualização da TZONE
        MODIFY tzone FROM i_tzone.
* Criar mensagem de sucesso
        ls_msg-type = 'S'.
        ls_msg-msg1 = 'Registro atualizar com sucesso na TZONE!'.
        APPEND ls_msg TO e_msg.

      CATCH cx_sy_sql_error INTO DATA(lx_sql).
* Criar mensagem de erro
        ls_msg-type = 'E'.
        ls_msg-msg1 = 'Erro ao atualizar a TZONE'.
        ls_msg-msg2 = lx_sql->get_text( ).
        APPEND ls_msg TO e_msg.
* ENQUEUE/DEQUEUE Tabela TZONE
        e_msg = enqueue_tzone( i_exec = abap_off ).
        RETURN. " Retorna imediatamente em caso de erro
    ENDTRY.
* ENQUEUE/DEQUEUE Tabela TZONE
    e_msg = enqueue_tzone( i_exec = abap_off ).

  ENDMETHOD.


  METHOD enqueue_tzone.
*&-------------------------------------------------------------------------------------------------------*
*& Método         : ATUALIZA_ZONA                                                                        *
*& Chamado        : USER STORY 171563                                                                    *
*& Data           : 03/04/2025                                                                           *
*& Especificado   : Paulo Quevedo                                                                        *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 04/04/2025|DEVK9A1XAW |NSEGATIN       | ZSDT0113 - Ajuste no cadastro de Roteiro.                     *
*--------------------------------------------------------------------------------------------------------*
    DATA: tl_sellist TYPE scprvimsellist,
          el_msg     TYPE ty_msg.

    CASE i_exec.
      WHEN abap_on.  "Enqueu
        CALL FUNCTION 'VIEW_ENQUEUE'
          EXPORTING
            view_name        = 'V_TZONE'
            action           = 'E'
            enqueue_mode     = 'E'
            enqueue_range    = ' '
          TABLES
            sellist          = tl_sellist
          EXCEPTIONS
            foreign_lock     = 1
            system_failure   = 2
            table_not_found  = 5
            client_reference = 7.

        CASE sy-subrc.
          WHEN 1.
            MESSAGE s049(sv) WITH sy-msgv1(12) INTO el_msg-msg2.

          WHEN 2.
            MESSAGE s050(sv) WITH 'V_TZONE' INTO el_msg-msg2.

          WHEN 5.
            MESSAGE s028(sv) WITH 'V_TZONE' INTO el_msg-msg2.

          WHEN 7.
            MESSAGE s054(sv) WITH sy-mandt INTO el_msg-msg2.

          WHEN OTHERS.
* Do nothing
        ENDCASE.

        CHECK NOT sy-subrc IS INITIAL.
* Criar mensagem de erro
        el_msg-type = 'E'.
        el_msg-msg1 = 'Erro ao inserir na TZONE'.
        APPEND el_msg TO e_msg.

      WHEN abap_off. "Dequeu
        CALL FUNCTION 'VIEW_ENQUEUE'
          EXPORTING
            view_name        = 'V_TZONE'
            action           = 'D'
            enqueue_mode     = 'E'
            enqueue_range    = ' '
          TABLES
            sellist          = tl_sellist
          EXCEPTIONS
            foreign_lock     = 1
            system_failure   = 2
            table_not_found  = 5
            client_reference = 7.

        CASE sy-subrc.
          WHEN 1.
            MESSAGE s049(sv) WITH sy-msgv1 INTO el_msg-msg2.

          WHEN 2.
            MESSAGE s050(sv) WITH 'V_TZONE' INTO el_msg-msg2.

          WHEN 5.
            MESSAGE s028(sv) WITH 'V_TZONE' INTO el_msg-msg2.

          WHEN 7.
            MESSAGE s054(sv) WITH sy-mandt INTO el_msg-msg2.

          WHEN OTHERS.
* Do nothing
        ENDCASE.

        CHECK NOT sy-subrc IS INITIAL.
* Criar mensagem de erro
        el_msg-type = 'E'.
        el_msg-msg1 = 'Erro ao inserir na TZONE'.
        APPEND el_msg TO e_msg.

      WHEN OTHERS.
*     Do nothing
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
