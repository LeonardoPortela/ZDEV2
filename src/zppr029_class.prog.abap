*&---------------------------------------------------------------------*
*& Include          ZPPR029_CLASS
*&---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* Interface..: LIF_TIPOS_GLOBAL
*---------------------------------------------------------------------*
* Descrição:
*  Define os tipos de dados utilizados em todo o programa.
*
* Responsabilidades:
*  - Declarar estruturas e tabelas internas reutilizáveis
*  - Centralizar os tipos relacionados a boletins, BKPF, BSEG, MCHB etc.
*
* Dependências:
*  - Estruturas ZPPS_*, ZMME_RETURN_SUCESS, ZMME_RETURN_ERROR
*
* Notas:
*  - Facilita o reaproveitamento e padronização entre classes
*---------------------------------------------------------------------*
INTERFACE lif_tipos_global.

  TYPES:
    BEGIN OF ty_zpps_ximfbf_log,
      obj_key    TYPE zpps_ximfbf_log-obj_key,
      nrobol     TYPE zpps_ximfbf_log-nrobol,
      fgorigem   TYPE zpps_ximfbf_log-fgorigem,
      werks      TYPE zpps_ximfbf_log-werks,
      matnr      TYPE zpps_ximfbf_log-matnr,
      charg      TYPE zpps_ximfbf_log-charg,
      mblnr      TYPE zpps_ximfbf_log-mblnr,
      cd_safra   TYPE zpps_ximfbf_log-cd_safra,
      fg_tpmovto TYPE zpps_ximfbf_log-fg_tpmovto,
      dtmvto     TYPE zpps_ximfbf_log-dtmvto,
      id_matgeo  TYPE zpps_ximfbf_log-id_matgeo,
      zst_atlz   TYPE zpps_ximfbf_log-zst_atlz,
      qteprod    TYPE zpps_ximfbf_log-qteprod,
      awkey      TYPE bkpf-awkey,
    END OF ty_zpps_ximfbf_log,

    BEGIN OF ty_bkpf,
      bukrs TYPE bkpf-bukrs,
      belnr TYPE bkpf-belnr,
      gjahr TYPE bkpf-gjahr,
      awkey TYPE bkpf-awkey,
    END OF ty_bkpf,

    BEGIN OF ty_bseg,
      bukrs TYPE bseg-bukrs,
      belnr TYPE bseg-belnr,
      gjahr TYPE bseg-gjahr,
      buzid TYPE bseg-buzid,
      shkzg TYPE bseg-shkzg,
      matnr TYPE bseg-matnr,
      dmbtr TYPE bseg-dmbtr,
      dmbe2 TYPE bseg-dmbe2,
      dmbe3 TYPE bseg-dmbe3,
    END OF ty_bseg,

    BEGIN OF ty_mchb,
      matnr TYPE mchb-matnr,
      werks TYPE mchb-werks,
      lgort TYPE mchb-lgort,
      charg TYPE mchb-charg,
      clabs TYPE mchb-clabs,
    END OF ty_mchb,

    ty_t_return_sucess TYPE TABLE OF zmme_return_sucess WITH DEFAULT KEY,
    ty_t_return_error  TYPE TABLE OF zmme_return_error  WITH DEFAULT KEY,

    BEGIN OF ty_sucesso,
      nrobol    TYPE zpps0016-nrobol,
      werks     TYPE zpps0016-werks,
      t_boletim TYPE ty_t_return_sucess,
    END OF ty_sucesso,

    BEGIN OF ty_erro,
      nrobol    TYPE zpps0016-nrobol,
      werks     TYPE zpps0016-werks,
      t_boletim TYPE ty_t_return_error,
    END OF ty_erro,

    ty_t_zpps_ximfbf_log TYPE TABLE OF ty_zpps_ximfbf_log,
    ty_t_bkpf            TYPE TABLE OF ty_bkpf,
    ty_t_bseg            TYPE TABLE OF ty_bseg,
    ty_t_mchb            TYPE TABLE OF ty_mchb,
    ty_t_saida           TYPE TABLE OF zpps0016,

    ty_t_sucesso         TYPE TABLE OF ty_sucesso,
    ty_t_erro            TYPE TABLE OF ty_erro,

    ty_tela              TYPE zpps_ximfbf_log,
    ty_r_nrobol          TYPE RANGE OF zpps_ximfbf_log-nrobol,
    ty_r_werks           TYPE RANGE OF zpps_ximfbf_log-werks,
    ty_r_dtmvto          TYPE RANGE OF zpps_ximfbf_log-dtmvto.

ENDINTERFACE.

*---------------------------------------------------------------------*
* Interface..: LIF_CONSTANTES_GLOBAL
*---------------------------------------------------------------------*
* Descrição:
*  Contém constantes globais utilizadas no programa.
*
* Responsabilidades:
*  - Armazenar os ícones de status (sucesso/erro) usados no ALV
*
* Dependências:
*  - Tipo ICON_D do SAP
*
* Notas:
*  - Usada principalmente para exibir ícones no relatório
*---------------------------------------------------------------------*
INTERFACE lif_constantes_global.

  CONSTANTS:
    c_icon_sucesso TYPE icon_d VALUE '@5B@',  " Verde
    c_icon_erro    TYPE icon_d VALUE '@5C@'.  " Vermelho

ENDINTERFACE.

*---------------------------------------------------------------------*
* Classe..: LCL_GERENCIADOR_DADOS
*---------------------------------------------------------------------*
* Descrição:
*  Responsável por buscar os dados necessários ao processo.
*
* Responsabilidades:
*  - Ler dados das tabelas ZPPS_XIMFBF_LOG, BKPF, BSEG e MCHB
*  - Preparar as informações para o processamento de boletins
*
* Dependências:
*  - Tabelas: ZPPS_XIMFBF_LOG, BKPF, BSEG, MCHB
*
* Notas:
*  - Usa FOR ALL ENTRIES com otimizações como binary search
*---------------------------------------------------------------------*
CLASS lcl_gerenciador_dados DEFINITION.

  PUBLIC SECTION.

    METHODS:
      constructor IMPORTING i_r_nrobol TYPE lif_tipos_global=>ty_r_nrobol
                            i_r_werks  TYPE lif_tipos_global=>ty_r_werks
                            i_r_dtmvto TYPE lif_tipos_global=>ty_r_dtmvto,
      iniciar.

    DATA:
      t_zpps_ximfbf_log TYPE lif_tipos_global=>ty_t_zpps_ximfbf_log READ-ONLY,
      t_bkpf            TYPE lif_tipos_global=>ty_t_bkpf            READ-ONLY,
      t_bseg            TYPE lif_tipos_global=>ty_t_bseg            READ-ONLY,
      t_mchb            TYPE lif_tipos_global=>ty_t_mchb            READ-ONLY.

  PRIVATE SECTION.

    METHODS:
      converter_dados,
      buscar_zpps_ximfbf_log,
      buscar_bkpf,
      buscar_bseg,
      buscar_mchb.

    DATA:
      r_nrobol TYPE lif_tipos_global=>ty_r_nrobol,
      r_werks  TYPE lif_tipos_global=>ty_r_werks,
      r_dtmvto TYPE lif_tipos_global=>ty_r_dtmvto.

ENDCLASS.

CLASS lcl_gerenciador_dados IMPLEMENTATION.

  METHOD constructor.

    me->r_nrobol = i_r_nrobol.
    me->r_werks  = i_r_werks.
    me->r_dtmvto = i_r_dtmvto.

  ENDMETHOD.

  METHOD iniciar.

    me->buscar_zpps_ximfbf_log( ).
    me->converter_dados( ).
    me->buscar_bkpf( ).
    me->buscar_bseg( ).
    me->buscar_mchb( ).

  ENDMETHOD.

  METHOD converter_dados.

    LOOP AT me->t_zpps_ximfbf_log ASSIGNING FIELD-SYMBOL(<fs_zpps_ximfbf_log>).

      CONCATENATE <fs_zpps_ximfbf_log>-mblnr
                  <fs_zpps_ximfbf_log>-dtmvto(4)
             INTO <fs_zpps_ximfbf_log>-awkey.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = <fs_zpps_ximfbf_log>-matnr
        IMPORTING
          output = <fs_zpps_ximfbf_log>-matnr.

    ENDLOOP.

  ENDMETHOD.

  METHOD buscar_zpps_ximfbf_log.

    SELECT obj_key
           nrobol
           fgorigem
           werks
           matnr
           charg
           mblnr
           cd_safra
           fg_tpmovto
           dtmvto
           id_matgeo
           zst_atlz
           qteprod
      FROM zpps_ximfbf_log
      INTO TABLE me->t_zpps_ximfbf_log
      WHERE nrobol IN me->r_nrobol
        AND werks  IN me->r_werks
        AND dtmvto IN me->r_dtmvto.

    IF sy-subrc IS INITIAL.

      SORT me->t_zpps_ximfbf_log BY nrobol werks.

    ELSE.

      " Dados não encontrados.
      MESSAGE ID 'ZPPM002'
              TYPE 'S'
              NUMBER '000'
              DISPLAY LIKE 'E'
              WITH TEXT-m01.


      LEAVE LIST-PROCESSING.

    ENDIF.

  ENDMETHOD.

  METHOD buscar_bkpf.

    IF me->t_zpps_ximfbf_log IS NOT INITIAL.

      SELECT bukrs
             belnr
             gjahr
             awkey
        FROM bkpf
        INTO TABLE me->t_bkpf
        FOR ALL ENTRIES IN me->t_zpps_ximfbf_log
        WHERE awkey = me->t_zpps_ximfbf_log-awkey.

      IF sy-subrc IS INITIAL.

        SORT me->t_bkpf BY awkey.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD buscar_bseg.

    IF me->t_bkpf IS NOT INITIAL.

      SELECT bukrs
             belnr
             gjahr
             buzid
             shkzg
             matnr
             dmbtr
             dmbe2
             dmbe3
        FROM bseg
        INTO TABLE me->t_bseg
        FOR ALL ENTRIES IN me->t_bkpf
        WHERE bukrs = me->t_bkpf-bukrs
          AND belnr = me->t_bkpf-belnr
          AND gjahr = me->t_bkpf-gjahr.

      IF me->t_bseg IS NOT INITIAL.

        SORT me->t_bseg BY bukrs belnr gjahr buzid shkzg matnr.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD buscar_mchb.

    IF me->t_zpps_ximfbf_log IS NOT INITIAL.

      SELECT matnr
             werks
             lgort
             charg
             clabs
        INTO TABLE me->t_mchb
        FROM mchb
        FOR ALL ENTRIES IN me->t_zpps_ximfbf_log
        WHERE matnr = me->t_zpps_ximfbf_log-matnr
          AND werks = me->t_zpps_ximfbf_log-werks
          AND lgort = me->t_zpps_ximfbf_log-cd_safra
          AND charg = me->t_zpps_ximfbf_log-charg.

      IF sy-subrc IS INITIAL.

        SORT me->t_mchb BY matnr werks lgort charg.

      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
* Interface..: LIF_CONTROLADOR_BOLETIM
*---------------------------------------------------------------------*
* Descrição:
*  Interface que define a estrutura mínima para controladores de boletins.
*
* Responsabilidades:
*  - Declarar os métodos que cada tipo de controlador deve implementar
*  - Garantir a uniformidade entre boletins de sucesso e erro
*
* Dependências:
*  - Tipos da interface LIF_TIPOS_GLOBAL
*
* Notas:
*  - Implementada por LCL_CONTROLADOR_BOLETIM e suas herdeiras
*---------------------------------------------------------------------*
INTERFACE lif_controlador_boletim.

  METHODS:
    preencher_envio_boletim IMPORTING i_s_zpps_ximfbf_log TYPE lif_tipos_global=>ty_zpps_ximfbf_log,
    enviar_boletins         IMPORTING i_t_sucesso TYPE lif_tipos_global=>ty_t_return_sucess OPTIONAL
                                      i_t_erro    TYPE lif_tipos_global=>ty_t_return_error  OPTIONAL,
    ordenar_dados.

ENDINTERFACE.

*---------------------------------------------------------------------*
* Classe..: LCL_CONTROLADOR_BOLETIM
*---------------------------------------------------------------------*
* Descrição:
*  Classe base abstrata para controladores de boletins.
*
* Responsabilidades:
*  - Definir a interface comum entre sucesso e erro
*  - Armazenar referência ao gerenciador de dados
*
* Dependências:
*  - LIF_CONTROLADOR_BOLETIM, LCL_GERENCIADOR_DADOS
*
* Notas:
*  - Utiliza aliases para facilitar o uso dos métodos da interface
*---------------------------------------------------------------------*
CLASS lcl_controlador_boletim DEFINITION ABSTRACT.

  PUBLIC SECTION.
    INTERFACES: lif_controlador_boletim ABSTRACT METHODS preencher_envio_boletim
      enviar_boletins
      ordenar_dados.

    ALIASES:
      preencher_envio_boletim FOR lif_controlador_boletim~preencher_envio_boletim,
      enviar_boletins         FOR lif_controlador_boletim~enviar_boletins,
      ordenar_dados           FOR lif_controlador_boletim~ordenar_dados.

    METHODS:
      constructor IMPORTING i_o_gerenciador_dados TYPE REF TO lcl_gerenciador_dados.

  PROTECTED SECTION.

    METHODS:
      inserir_log IMPORTING i_v_idbol    TYPE zppt0041-idbol
                            i_v_nrobol   TYPE zppt0041-nrobol
                            i_v_tp_envio TYPE zppt0041-tp_envio,
      salvar_log.

    DATA:
      o_gerenciador_dados TYPE REF TO lcl_gerenciador_dados,
      t_log               TYPE TABLE OF zppt0041.


ENDCLASS.

CLASS lcl_controlador_boletim IMPLEMENTATION.

  METHOD constructor.

    me->o_gerenciador_dados = i_o_gerenciador_dados.

  ENDMETHOD.


  METHOD inserir_log.

    DATA:
          ls_log TYPE zppt0041.

    TRY.

        ls_log-id_log   = cl_system_uuid=>create_uuid_x16_static( ).
        ls_log-idbol    = i_v_idbol.
        ls_log-nrobol   = i_v_nrobol.
        ls_log-uname    = sy-uname.
        ls_log-datum    = sy-datum.
        ls_log-uzeit    = sy-uzeit.
        ls_log-tp_envio = i_v_tp_envio.

        APPEND ls_log TO me->t_log.

      CATCH cx_root.
        " Ocorreu um erro...
    ENDTRY.

  ENDMETHOD.


  METHOD salvar_log.

    IF me->t_log IS NOT INITIAL.

      MODIFY zppt0041 FROM TABLE me->t_log.
      IF sy-subrc IS INITIAL.

        COMMIT WORK AND WAIT.

      ENDIF.

      CLEAR me->t_log.

    ENDIF.

  ENDMETHOD.


ENDCLASS.

*---------------------------------------------------------------------*
* Classe..: LCL_BOLETIM_SUCESSO
*---------------------------------------------------------------------*
* Descrição:
*  Trata boletins válidos prontos para envio com sucesso.
*
* Responsabilidades:
*  - Preencher os dados da estrutura de sucesso
*  - Buscar valores contábeis (BSEG/BKPF)
*  - Enviar boletins válidos via função Z
*
* Dependências:
*  - Z_MM_OUTBOUND_BOL_SUCESS, LCL_CONTROLADOR_BOLETIM
*
* Notas:
*  - Verifica tipo de movimento e monta estrutura ZMME_RETURN_SUCESS
*---------------------------------------------------------------------*
CLASS lcl_boletim_sucesso DEFINITION INHERITING FROM lcl_controlador_boletim.

  PUBLIC SECTION.
    METHODS:
      preencher_envio_boletim REDEFINITION,
      enviar_boletins         REDEFINITION,
      ordenar_dados           REDEFINITION,
      get_items_boletim IMPORTING i_s_saida          TYPE zpps0016
                        RETURNING VALUE(r_t_boletim) TYPE lif_tipos_global=>ty_t_return_sucess.

  PRIVATE SECTION.

    DATA:
        t_sucesso TYPE lif_tipos_global=>ty_t_sucesso.

    METHODS:
      buscar_montantes IMPORTING i_s_zpps_ximfbf_log TYPE lif_tipos_global=>ty_zpps_ximfbf_log
                       CHANGING  c_s_boletim         TYPE zmme_return_sucess.

ENDCLASS.

CLASS lcl_boletim_sucesso IMPLEMENTATION.

  METHOD preencher_envio_boletim.

    DATA:
          ls_boletim TYPE zmme_return_sucess.

    ls_boletim-idbol     = i_s_zpps_ximfbf_log-obj_key.
    ls_boletim-nrobol    = i_s_zpps_ximfbf_log-nrobol.
    ls_boletim-fgorigem  = i_s_zpps_ximfbf_log-fgorigem.
    ls_boletim-bwart     = i_s_zpps_ximfbf_log-fg_tpmovto.
    ls_boletim-erdat     = i_s_zpps_ximfbf_log-dtmvto.
    ls_boletim-erzet     = sy-uzeit.
    ls_boletim-id_matgeo = i_s_zpps_ximfbf_log-id_matgeo.
    ls_boletim-matnr     = i_s_zpps_ximfbf_log-matnr.
    ls_boletim-docmat    = i_s_zpps_ximfbf_log-mblnr.

    me->buscar_montantes(
      EXPORTING
        i_s_zpps_ximfbf_log = i_s_zpps_ximfbf_log
      CHANGING
        c_s_boletim = ls_boletim ).

    ls_boletim-dtproc    = sy-datum.
    ls_boletim-hrproc    = sy-uzeit.

    READ TABLE me->t_sucesso ASSIGNING FIELD-SYMBOL(<fs_sucesso>)
                             WITH KEY nrobol = i_s_zpps_ximfbf_log-nrobol
                                      werks  = i_s_zpps_ximfbf_log-werks.
    IF sy-subrc IS NOT INITIAL.

      CLEAR sy-subrc.

      APPEND INITIAL LINE TO me->t_sucesso ASSIGNING <fs_sucesso>.
      IF sy-subrc IS INITIAL.

        <fs_sucesso>-nrobol = i_s_zpps_ximfbf_log-nrobol.
        <fs_sucesso>-werks  = i_s_zpps_ximfbf_log-werks.

      ENDIF.

    ENDIF.

    APPEND ls_boletim TO <fs_sucesso>-t_boletim.

  ENDMETHOD.

  METHOD buscar_montantes.

    DATA:
      lv_shkzg TYPE bseg-shkzg,
      lv_buzid TYPE bseg-buzid.

    IF i_s_zpps_ximfbf_log-zst_atlz = 'M' .
      lv_shkzg = 'S'. "Movto 261 - Crédito
      lv_buzid = 'S'.
    ELSEIF i_s_zpps_ximfbf_log-fg_tpmovto EQ '261'.
      lv_shkzg = 'H'. "Movto 261 - Débito
      lv_buzid = 'M'.
    ELSE.
      lv_shkzg = 'S'. "Movto 261 - Débito
      lv_buzid = 'M'.
    ENDIF.

    READ TABLE me->o_gerenciador_dados->t_bkpf INTO DATA(ls_bkpf)
                                               WITH KEY awkey = i_s_zpps_ximfbf_log-awkey
                                                                            BINARY SEARCH.

    IF sy-subrc IS INITIAL.

      READ TABLE me->o_gerenciador_dados->t_bseg INTO DATA(ls_bseg)
                                                 WITH KEY bukrs = ls_bkpf-bukrs
                                                          belnr = ls_bkpf-belnr
                                                          gjahr = ls_bkpf-gjahr
                                                          buzid = lv_buzid
                                                          shkzg = lv_shkzg
                                                          matnr = i_s_zpps_ximfbf_log-matnr
                                                                              BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        c_s_boletim-dmbtr = ls_bseg-dmbtr.
        c_s_boletim-dmbe2 = ls_bseg-dmbe2.
        c_s_boletim-dmbe3 = ls_bseg-dmbe3.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD enviar_boletins.

    IF i_t_sucesso IS NOT INITIAL.

      CALL FUNCTION 'Z_MM_OUTBOUND_BOL_SUCESS'
        TABLES
          return_sucess = i_t_sucesso.

      COMMIT WORK.


      DATA(lt_sucesso) = i_t_sucesso.

      SORT lt_sucesso BY idbol nrobol.
      DELETE ADJACENT DUPLICATES FROM lt_sucesso COMPARING idbol nrobol.

      LOOP AT lt_sucesso INTO DATA(ls_sucesso).

        me->inserir_log(
          i_v_idbol    = ls_sucesso-idbol
          i_v_nrobol   = ls_sucesso-nrobol
          i_v_tp_envio = 'S'
        ).

      ENDLOOP.

      me->salvar_log( ).

    ENDIF.

  ENDMETHOD.

  METHOD get_items_boletim.

    DATA:
          lt_boletim TYPE lif_tipos_global=>ty_t_sucesso.

    READ TABLE me->t_sucesso INTO DATA(ls_sucesso)
                          WITH KEY nrobol = i_s_saida-nrobol
                                   werks  = i_s_saida-werks
                                              BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      r_t_boletim = ls_sucesso-t_boletim.
    ENDIF.

  ENDMETHOD.

  METHOD ordenar_dados.

    SORT me->t_sucesso BY nrobol werks.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
* Classe..: LCL_REMOVEDOR_CARACTERE
*---------------------------------------------------------------------*
* Descrição:
*  Classe utilitária para manipulação de strings, removendo zeros à esquerda.
*
* Responsabilidades:
*  - Fornecer método para limpar zeros não significativos em strings numéricas
*
* Dependências:
*  - Nenhuma direta
*
* Notas:
*  - Utiliza o comando UNPACK para conversão automática
*---------------------------------------------------------------------*
CLASS lcl_removedor_caractere DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      remover_zero_esquerda IMPORTING i_v_valor        TYPE string
                            RETURNING VALUE(r_v_valor) TYPE string.

ENDCLASS.

CLASS lcl_removedor_caractere IMPLEMENTATION.

  METHOD remover_zero_esquerda.

    TRY.

        DATA(lv_valor) = i_v_valor.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = lv_valor
          IMPORTING
            output = r_v_valor.

      CATCH cx_root.

        r_v_valor = i_v_valor.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
* Classe..: LCX_BOLETIM_ERRO
*---------------------------------------------------------------------*
* Descrição:
*  Exceção genérica para falhas no processamento de boletins.
*
* Responsabilidades:
*  - Armazenar mensagem e parâmetros de erro
*  - Permitir leitura e exibição amigável do erro
*
* Dependências:
*  - Nenhuma direta
*
* Notas:
*  - Base para exceções específicas do domínio
*---------------------------------------------------------------------*
CLASS lcx_boletim_erro DEFINITION INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    DATA: id       TYPE sy-msgid,
          nromsg   TYPE sy-msgno,
          mensagem TYPE string,
          msgv1    TYPE string,
          msgv2    TYPE string,
          msgv3    TYPE string,
          msgv4    TYPE string.

    METHODS:
      constructor IMPORTING i_id     TYPE sy-msgid OPTIONAL
                            i_nromsg TYPE sy-msgno OPTIONAL
                            i_msg    TYPE string   OPTIONAL
                            i_msgv1  TYPE string   OPTIONAL
                            i_msgv2  TYPE string   OPTIONAL
                            i_msgv3  TYPE string   OPTIONAL
                            i_msgv4  TYPE string   OPTIONAL.
ENDCLASS.

CLASS lcx_boletim_erro IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    me->id       = i_id.
    me->nromsg   = i_nromsg.
    me->mensagem = i_msg.
    me->msgv1    = i_msgv1.
    me->msgv2    = i_msgv2.
    me->msgv3    = i_msgv3.
    me->msgv4    = i_msgv4.

    IF me->id IS INITIAL.
      me->id = 'ZPPM002'.
    ENDIF.

    IF me->nromsg IS INITIAL.
      me->nromsg = '000'.
    ENDIF.

    IF me->mensagem IS INITIAL.

      MESSAGE ID me->id
              TYPE 'E'
              NUMBER me->nromsg
              WITH me->msgv1
                   me->msgv2
                   me->msgv3
                   me->msgv4
              INTO me->mensagem.

    ENDIF.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
* Classe..: LCX_ESTOQUE_INSUFICIENTE
*---------------------------------------------------------------------*
* Descrição:
*  Exceção usada quando não há saldo suficiente para o boletim.
*
* Responsabilidades:
*  - Lançar erro com o código do material
*
* Dependências:
*  - LCLX_BOLETIM_ERRO
*
* Notas:
*  - Verifica campo CLABS da MCHB
*---------------------------------------------------------------------*
CLASS lcx_estoque_insuficiente DEFINITION INHERITING FROM lcx_boletim_erro.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING i_id    TYPE sy-msgid OPTIONAL
                            i_msg   TYPE string   OPTIONAL
                            i_msgv1 TYPE string   OPTIONAL
                            i_msgv2 TYPE string   OPTIONAL
                            i_msgv3 TYPE string   OPTIONAL
                            i_msgv4 TYPE string   OPTIONAL.

ENDCLASS.

CLASS lcx_estoque_insuficiente IMPLEMENTATION.

  METHOD constructor.
    DATA:
          lv_msgv1 TYPE string.

    IF 1 = 2.
      MESSAGE e001(zppm002) WITH i_msgv1.
    ENDIF.

    lv_msgv1 = lcl_removedor_caractere=>remover_zero_esquerda( i_msgv1 ).

    super->constructor(
      i_id     = i_id
      i_nromsg = '001'
      i_msg    = i_msg
      i_msgv1  = lv_msgv1
      i_msgv2  = i_msgv2
      i_msgv3  = i_msgv3
      i_msgv4  = i_msgv4
    ).

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
* Classe..: LCX_MCHB_NAO_ENCONTRADO
*---------------------------------------------------------------------*
* Descrição:
*  Exceção usada quando o lote informado não existe.
*
* Responsabilidades:
*  - Informar material e lote ausentes
*
* Dependências:
*  - LCLX_BOLETIM_ERRO
*
* Notas:
*  - Gatilho ao não encontrar registro na MCHB
*---------------------------------------------------------------------*
CLASS lcx_mchb_nao_encontrado DEFINITION INHERITING FROM lcx_boletim_erro.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING i_id    TYPE sy-msgid OPTIONAL
                            i_msg   TYPE string   OPTIONAL
                            i_msgv1 TYPE string   OPTIONAL
                            i_msgv2 TYPE string   OPTIONAL
                            i_msgv3 TYPE string   OPTIONAL
                            i_msgv4 TYPE string   OPTIONAL.

ENDCLASS.

CLASS lcx_mchb_nao_encontrado IMPLEMENTATION.

  METHOD constructor.
    DATA:
      lv_msgv2 TYPE string.

    IF 1 = 2.
      MESSAGE e002(zppm002) WITH i_msgv1 i_msgv2.
    ENDIF.

    lv_msgv2 = lcl_removedor_caractere=>remover_zero_esquerda( i_msgv2 ).

    super->constructor(
      i_id     = i_id
      i_nromsg = '002'
      i_msg    = i_msg
      i_msgv1  = i_msgv1
      i_msgv2  = lv_msgv2
      i_msgv3  = i_msgv3
      i_msgv4  = i_msgv4
    ).

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
* Classe..: LCX_ERRO_GENERICO
*---------------------------------------------------------------------*
* Descrição:
*  Exceção genérica para falhas não identificadas especificamente.
*
* Responsabilidades:
*  - Interromper o processamento em caso de erro inesperado
*
* Dependências:
*  - LCLX_BOLETIM_ERRO
*
* Notas:
*  - Utilizada como fallback no tratamento de erros
*---------------------------------------------------------------------*
CLASS lcx_erro_generico DEFINITION INHERITING FROM lcx_boletim_erro.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING i_id    TYPE sy-msgid OPTIONAL
                            i_msg   TYPE string   OPTIONAL
                            i_msgv1 TYPE string   OPTIONAL
                            i_msgv2 TYPE string   OPTIONAL
                            i_msgv3 TYPE string   OPTIONAL
                            i_msgv4 TYPE string   OPTIONAL.

ENDCLASS.

CLASS lcx_erro_generico IMPLEMENTATION.

  METHOD constructor.

    IF 1 = 2.
      MESSAGE e003(zppm002).
    ENDIF.

    super->constructor(
      i_id     = i_id
      i_nromsg = '003'
      i_msg    = i_msg
      i_msgv1  = i_msgv1
      i_msgv2  = i_msgv2
      i_msgv3  = i_msgv3
      i_msgv4  = i_msgv4
    ).

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
* Classe..: LCL_BOLETIM_ERRO
*---------------------------------------------------------------------*
* Descrição:
*  Trata boletins com erros que impedem o envio regular.
*
* Responsabilidades:
*  - Verificar erros de estoque ou lote inexistente
*  - Preencher estrutura de erro com mensagens
*  - Enviar boletins com erro via função Z
*
* Dependências:
*  - Z_MM_OUTBOUND_BOL_ERROR, LCL_CONTROLADOR_BOLETIM, LCLX_* exceptions
*
* Notas:
*  - Usa exceções para identificar causas específicas de erro
*---------------------------------------------------------------------*
CLASS lcl_boletim_erro DEFINITION INHERITING FROM lcl_controlador_boletim.

  PUBLIC SECTION.
    METHODS:
      preencher_envio_boletim REDEFINITION,
      enviar_boletins         REDEFINITION,
      ordenar_dados           REDEFINITION,
      get_items_boletim IMPORTING i_s_saida          TYPE zpps0016
                        RETURNING VALUE(r_t_boletim) TYPE lif_tipos_global=>ty_t_return_error.

    DATA:
      t_erro TYPE lif_tipos_global=>ty_t_erro READ-ONLY.

  PRIVATE SECTION.

    METHODS:
      verificar_erro IMPORTING i_s_zpps_ximfbf_log TYPE lif_tipos_global=>ty_zpps_ximfbf_log
                     CHANGING  c_s_boletim         TYPE zmme_return_error,
      verificar_mchb IMPORTING i_s_zpps_ximfbf_log TYPE lif_tipos_global=>ty_zpps_ximfbf_log
                     RAISING   lcx_boletim_erro,
      chamar_erro_generico RAISING lcx_boletim_erro.

ENDCLASS.

CLASS lcl_boletim_erro IMPLEMENTATION.

  METHOD preencher_envio_boletim.

    DATA:
          ls_boletim TYPE zmme_return_error.

    ls_boletim-idbol      = i_s_zpps_ximfbf_log-obj_key.
    ls_boletim-nrobol     = i_s_zpps_ximfbf_log-nrobol.
    ls_boletim-fgorigem   = i_s_zpps_ximfbf_log-fgorigem.
    ls_boletim-bwart      = i_s_zpps_ximfbf_log-fg_tpmovto.
    ls_boletim-erdat      = i_s_zpps_ximfbf_log-dtmvto.
    ls_boletim-erzet      = sy-uzeit.
    ls_boletim-type       = 'E'.

    me->verificar_erro(
      EXPORTING
        i_s_zpps_ximfbf_log = i_s_zpps_ximfbf_log
      CHANGING
        c_s_boletim         = ls_boletim ).

    ls_boletim-dtproc     = sy-datum.
    ls_boletim-hrproc     = sy-uzeit.

    READ TABLE me->t_erro ASSIGNING FIELD-SYMBOL(<fs_erro>)
                             WITH KEY nrobol = i_s_zpps_ximfbf_log-nrobol
                                      werks  = i_s_zpps_ximfbf_log-werks.
    IF sy-subrc IS NOT INITIAL.

      CLEAR sy-subrc.

      APPEND INITIAL LINE TO me->t_erro ASSIGNING <fs_erro>.
      IF sy-subrc IS INITIAL.

        <fs_erro>-nrobol = i_s_zpps_ximfbf_log-nrobol.
        <fs_erro>-werks  = i_s_zpps_ximfbf_log-werks.

      ENDIF.

    ENDIF.

    APPEND ls_boletim TO <fs_erro>-t_boletim.

  ENDMETHOD.

  METHOD verificar_erro.

    TRY.

        me->verificar_mchb( i_s_zpps_ximfbf_log ).
        me->chamar_erro_generico( ).

      CATCH lcx_boletim_erro INTO DATA(erro).

        c_s_boletim-id         = erro->id.
        c_s_boletim-nromsg     = erro->nromsg.
        c_s_boletim-message    = erro->mensagem.
        c_s_boletim-message_v1 = erro->msgv1.
        c_s_boletim-message_v2 = erro->msgv2.
        c_s_boletim-message_v3 = erro->msgv3.
        c_s_boletim-message_v4 = erro->msgv4.

    ENDTRY.

  ENDMETHOD.

  METHOD verificar_mchb.

    READ TABLE me->o_gerenciador_dados->t_mchb INTO DATA(ls_mchb)
                                               WITH KEY matnr = i_s_zpps_ximfbf_log-matnr
                                                        werks = i_s_zpps_ximfbf_log-werks
                                                        lgort = i_s_zpps_ximfbf_log-cd_safra
                                                        charg = i_s_zpps_ximfbf_log-charg
                                                                               BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      IF i_s_zpps_ximfbf_log-qteprod > ls_mchb-clabs.

        " Saldo insuficiente para o Material & informado
        RAISE EXCEPTION TYPE lcx_estoque_insuficiente
          EXPORTING
            i_msgv1 = CONV string( i_s_zpps_ximfbf_log-matnr ).

      ENDIF.

    ELSE.

      " Lote & informado para o material & não encontrado
      RAISE EXCEPTION TYPE lcx_mchb_nao_encontrado
        EXPORTING
          i_msgv1 = CONV string( i_s_zpps_ximfbf_log-charg )
          i_msgv2 = CONV string( i_s_zpps_ximfbf_log-matnr ).

    ENDIF.

  ENDMETHOD.

  METHOD chamar_erro_generico.

    " Erro ao processar o Boletim
    RAISE EXCEPTION TYPE lcx_erro_generico.

  ENDMETHOD.

  METHOD enviar_boletins.

    IF i_t_erro IS NOT INITIAL.

      CALL FUNCTION 'Z_MM_OUTBOUND_BOL_ERROR'
        TABLES
          return_error = i_t_erro.

      COMMIT WORK.

      DATA(lt_erro) = i_t_erro.

      SORT lt_erro BY idbol nrobol.
      DELETE ADJACENT DUPLICATES FROM lt_erro COMPARING idbol nrobol.

      LOOP AT lt_erro INTO DATA(ls_erro).

        me->inserir_log(
          i_v_idbol    = ls_erro-idbol
          i_v_nrobol   = ls_erro-nrobol
          i_v_tp_envio = 'E'
        ).

      ENDLOOP.

      me->salvar_log( ).

    ENDIF.

  ENDMETHOD.

  METHOD get_items_boletim.

    DATA:
          lt_boletim TYPE lif_tipos_global=>ty_t_return_error.

    READ TABLE me->t_erro INTO DATA(ls_erro)
                          WITH KEY nrobol = i_s_saida-nrobol
                                   werks  = i_s_saida-werks
                                               BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      r_t_boletim = ls_erro-t_boletim.
    ENDIF.

  ENDMETHOD.

  METHOD ordenar_dados.

    SORT me->t_erro BY nrobol werks.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
* Classe..: LCL_PROCESSADOR_DADOS
*---------------------------------------------------------------------*
* Descrição:
*  Gerencia o processamento e controle dos boletins.
*
* Responsabilidades:
*  - Definir o tipo de envio (sucesso/erro)
*  - Delegar o preenchimento dos dados aos controladores
*  - Expor os resultados para o relatório
*
* Dependências:
*  - LCL_CONTROLADOR_BOLETIM, LCL_BOLETIM_SUCESSO, LCL_BOLETIM_ERRO
*
* Notas:
*  - Usa o campo MBLNR para definir o tipo de boletim
*---------------------------------------------------------------------*
CLASS lcl_processador_dados DEFINITION.

  PUBLIC SECTION.

    DATA:
          t_saida TYPE lif_tipos_global=>ty_t_saida READ-ONLY.


    METHODS:
      constructor IMPORTING i_o_gerenciador_dados TYPE REF TO lcl_gerenciador_dados,
      iniciar,
      get_o_boletim_sucesso RETURNING VALUE(r_o_boletim_sucesso) TYPE REF TO lcl_boletim_sucesso,
      get_o_boletim_erro    RETURNING VALUE(r_o_boletim_erro)    TYPE REF TO lcl_boletim_erro.

  PRIVATE SECTION.

    DATA:
      o_gerenciador_dados TYPE REF TO lcl_gerenciador_dados,
      o_boletim_sucesso   TYPE REF TO lcl_boletim_sucesso,
      o_boletim_erro      TYPE REF TO lcl_boletim_erro.

    METHODS:
      definir_tipo_envio IMPORTING i_v_mblnr                      TYPE zpps_ximfbf_log-mblnr
                         EXPORTING e_v_envio                      TYPE zpps0016-envio
                         RETURNING VALUE(r_o_controlador_boletim) TYPE REF TO lif_controlador_boletim.

ENDCLASS.

CLASS lcl_processador_dados IMPLEMENTATION.

  METHOD constructor.

    me->o_gerenciador_dados = i_o_gerenciador_dados.

    CREATE OBJECT me->o_boletim_sucesso
      EXPORTING
        i_o_gerenciador_dados = me->o_gerenciador_dados.

    CREATE OBJECT me->o_boletim_erro
      EXPORTING
        i_o_gerenciador_dados = me->o_gerenciador_dados.

  ENDMETHOD.

  METHOD iniciar.

    DATA:
      ls_saida               TYPE zpps0016,
      lt_cabecalho           TYPE lif_tipos_global=>ty_t_zpps_ximfbf_log,
      lo_controlador_boletim TYPE REF TO lif_controlador_boletim.

    lt_cabecalho = me->o_gerenciador_dados->t_zpps_ximfbf_log.

    SORT lt_cabecalho BY nrobol werks.
    DELETE ADJACENT DUPLICATES FROM lt_cabecalho COMPARING nrobol werks.

    LOOP AT lt_cabecalho INTO DATA(ls_cabecalho).

      READ TABLE me->o_gerenciador_dados->t_zpps_ximfbf_log TRANSPORTING NO FIELDS
                                                            WITH KEY nrobol = ls_cabecalho-nrobol
                                                                     werks  = ls_cabecalho-werks
                                                                                   BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        ls_saida-nrobol = ls_cabecalho-nrobol.
        ls_saida-werks  = ls_cabecalho-werks.

        lo_controlador_boletim ?= me->definir_tipo_envio(
                                        EXPORTING
                                          i_v_mblnr = ls_cabecalho-mblnr
                                        IMPORTING
                                          e_v_envio = ls_saida-envio
                                       ).

        LOOP AT me->o_gerenciador_dados->t_zpps_ximfbf_log INTO DATA(ls_zpps_ximfbf_log) FROM sy-tabix.

          IF ls_cabecalho-nrobol NE ls_zpps_ximfbf_log-nrobol
          OR ls_cabecalho-werks  NE ls_zpps_ximfbf_log-werks.

            EXIT.

          ENDIF.

          lo_controlador_boletim->preencher_envio_boletim(
            EXPORTING
              i_s_zpps_ximfbf_log = ls_zpps_ximfbf_log
           ).

        ENDLOOP.

      ENDIF.

      APPEND ls_saida TO me->t_saida.

    ENDLOOP.

    me->o_boletim_erro->ordenar_dados( ).
    me->o_boletim_sucesso->ordenar_dados( ).

  ENDMETHOD.

  METHOD definir_tipo_envio.

    IF i_v_mblnr IS NOT INITIAL.

      r_o_controlador_boletim = me->o_boletim_sucesso.
      e_v_envio = lif_constantes_global=>c_icon_sucesso.

    ELSE.

      r_o_controlador_boletim = me->o_boletim_erro.
      e_v_envio = lif_constantes_global=>c_icon_erro.

    ENDIF.

  ENDMETHOD.

  METHOD get_o_boletim_sucesso.

    r_o_boletim_sucesso = me->o_boletim_sucesso.

  ENDMETHOD.

  METHOD get_o_boletim_erro.

    r_o_boletim_erro = me->o_boletim_erro.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
* Classe..: LCL_BOTOES
*---------------------------------------------------------------------*
* Descrição:
*  Define e centraliza os identificadores dos botões usados no ALV.
*
* Responsabilidades:
*  - Registrar os nomes técnicos dos botões da toolbar
*
* Dependências:
*  - Nenhuma
*
* Notas:
*  - Facilita manutenção dos nomes dos botões
*---------------------------------------------------------------------*
CLASS lcl_botoes DEFINITION.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_botoes,
             envio          TYPE salv_de_function,
             enviar_boletim TYPE salv_de_function,
             voltar         TYPE salv_de_function,
             sair           TYPE salv_de_function,
             cancelar       TYPE salv_de_function,
           END OF ty_botoes.

    CLASS-DATA:
      botao TYPE ty_botoes READ-ONLY.

    CLASS-METHODS:
      class_constructor.

ENDCLASS.

CLASS lcl_botoes IMPLEMENTATION.

  METHOD class_constructor.

    botao-envio          = 'ENVIO'.
    botao-enviar_boletim = 'ENVIAR_BOL'.
    botao-voltar         = 'BACK'.
    botao-sair           = 'EXIT'.
    botao-cancelar       = 'CANC'.

  ENDMETHOD.

ENDCLASS.


*---------------------------------------------------------------------*
* Classe..: LCL_POPUP_CONFIRMACAO
*---------------------------------------------------------------------*
* Descrição:
*  Classe utilitária para exibir popups de confirmação ao usuário.
*
* Responsabilidades:
*  - Apresentar uma janela de confirmação antes de ações críticas
*  - Retornar a escolha do usuário como valor booleano
*
* Dependências:
*  - Função POPUP_TO_CONFIRM
*
* Notas:
*  - Utilizada antes do envio de boletins para o GEO
*---------------------------------------------------------------------*
CLASS lcl_popup_confirmacao DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      confirmar_envio_boletim RETURNING VALUE(r_v_confirmado) TYPE boolean.

ENDCLASS.

CLASS lcl_popup_confirmacao IMPLEMENTATION.

  METHOD confirmar_envio_boletim.

    DATA: lv_resposta TYPE c.

    TRY.

        r_v_confirmado = abap_false.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = CONV string( TEXT-p01 )
            text_question         = CONV string( TEXT-p02 )
            text_button_1         = CONV string( TEXT-p03 )
            text_button_2         = CONV string( TEXT-p04 )
            default_button        = '2'
            display_cancel_button = abap_false
          IMPORTING
            answer                = lv_resposta.

        IF lv_resposta = '1'.
          r_v_confirmado = abap_true.
        ENDIF.

      CATCH cx_root.
        " Erro ao gerar popup...
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
* Classe..: LCL_RELATORIO
*---------------------------------------------------------------------*
* Descrição:
*  Classe abstrata base para construção dos relatórios ALV.
*
* Responsabilidades:
*  - Fornecer estrutura para exibição de dados via CL_SALV_TABLE
*  - Definir botões, eventos e configurações comuns aos ALVs
*
* Dependências:
*  - CL_SALV_TABLE, LCL_PROCESSADOR_DADOS, LCL_BOTOES
*
* Notas:
*  - Deve ser herdada pelas classes específicas de sucesso, erro
*    e relatório principal.
*  - Irão herdar com status gui ZSTATUS_BOLETIM
*---------------------------------------------------------------------*
CLASS lcl_relatorio DEFINITION ABSTRACT.

  PUBLIC SECTION.

    METHODS:
      constructor IMPORTING i_o_processador_dados TYPE REF TO lcl_processador_dados,
      exibir.

  PROTECTED SECTION.
    DATA:
      v_pf_status         TYPE sypfkey,
      o_alv               TYPE REF TO cl_salv_table,
      o_processador_dados TYPE REF TO lcl_processador_dados.

    METHODS:
      acionar_botao FOR EVENT added_function  OF cl_salv_events_table
        IMPORTING e_salv_function,
      enviar_boletins_para_geo ABSTRACT,
      adicionar_evento,
      adicionar_botao,
      configurar_alv,

      get_o_alv RETURNING VALUE(r_o_alv) TYPE REF TO cl_salv_table.

ENDCLASS.

CLASS lcl_relatorio IMPLEMENTATION.

  METHOD constructor.

    me->v_pf_status = 'ZSTATUS_BOLETIM'.
    me->o_processador_dados = i_o_processador_dados.

  ENDMETHOD.

  METHOD exibir.

    me->o_alv->display( ).

  ENDMETHOD.

  METHOD configurar_alv.

    " Ativar padrão zebra
    DATA(lo_display) = me->o_alv->get_display_settings( ).
    lo_display->set_striped_pattern( abap_true ).

    " Habilitar filtros
    DATA(lo_functions) = me->o_alv->get_functions( ).
    lo_functions->set_all( abap_true ).

    " Permitir seleção
    DATA(lo_sel) = me->o_alv->get_selections( ).
    lo_sel->set_selection_mode( if_salv_c_selection_mode=>row_column ).

    " Otimizar largura das colunas automaticamente
    DATA(lo_columns) = me->o_alv->get_columns( ).
    lo_columns->set_optimize( abap_true ).

  ENDMETHOD.

  METHOD adicionar_botao.

    me->o_alv->set_screen_status(
      report        = sy-repid
      pfstatus      = me->v_pf_status
      set_functions = cl_salv_table=>c_functions_all
    ).

  ENDMETHOD.

  METHOD acionar_botao.

    CASE e_salv_function.

      WHEN lcl_botoes=>botao-enviar_boletim.
        me->enviar_boletins_para_geo( ).

      WHEN lcl_botoes=>botao-voltar.
        LEAVE TO SCREEN 0.

      WHEN lcl_botoes=>botao-sair OR lcl_botoes=>botao-cancelar.
        LEAVE PROGRAM.

      WHEN OTHERS.
        " Função não esperada...

    ENDCASE.

  ENDMETHOD.

  METHOD get_o_alv.

    r_o_alv = me->o_alv.

  ENDMETHOD.

  METHOD adicionar_evento.

    DATA(lo_events) = me->o_alv->get_event( ).
    DATA(lo_event_table) = CAST cl_salv_events_table( lo_events ).

    SET HANDLER me->acionar_botao FOR lo_event_table.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
* Classe..: LCL_RELATORIO_SUCESSO
*---------------------------------------------------------------------*
* Descrição:
*  Classe de relatório para exibir boletins enviados com sucesso.
*
* Responsabilidades:
*  - Exibir ALV com boletins de sucesso
*  - Permitir seleção e envio de registros ao sistema GEO
*
* Dependências:
*  - LCL_RELATORIO, CL_SALV_TABLE, LCL_POPUP_CONFIRMACAO
*
* Notas:
*  - Usa o status-pf ZSTATUS_BOLETIM
*---------------------------------------------------------------------*
CLASS lcl_relatorio_sucesso DEFINITION
                            INHERITING FROM lcl_relatorio.

  PUBLIC SECTION.

    METHODS:
      constructor IMPORTING i_o_processador_dados TYPE REF TO lcl_processador_dados
                            i_t_sucesso           TYPE lif_tipos_global=>ty_t_return_sucess.

  PROTECTED SECTION.
    METHODS:
      enviar_boletins_para_geo REDEFINITION.

  PRIVATE SECTION.
    DATA:
          t_sucesso TYPE lif_tipos_global=>ty_t_return_sucess.

ENDCLASS.

CLASS lcl_relatorio_sucesso IMPLEMENTATION.

  METHOD constructor.

    super->constructor( i_o_processador_dados ).
    me->t_sucesso   = i_t_sucesso.

    TRY.

        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = me->o_alv
          CHANGING
            t_table      = me->t_sucesso ).

        me->configurar_alv( ).
        me->adicionar_botao( ).
        me->adicionar_evento( ).

      CATCH cx_root.
        " Ocorreu um erro ao gerar o ALV...
    ENDTRY.

  ENDMETHOD.

  METHOD enviar_boletins_para_geo.

    DATA: lt_rows           TYPE salv_t_row,
          lv_index          TYPE sytabix,
          lt_enviar_sucesso TYPE lif_tipos_global=>ty_t_return_sucess.

    lt_rows = me->o_alv->get_selections( )->get_selected_rows( ).

    IF lt_rows IS INITIAL.
      " Selecione pelo menos um registro
      MESSAGE TEXT-t03 TYPE 'S'.
      RETURN.
    ENDIF.

    IF lcl_popup_confirmacao=>confirmar_envio_boletim( ) IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_rows INTO lv_index.

      READ TABLE me->t_sucesso INTO DATA(ls_sucesso) INDEX lv_index.
      IF sy-subrc IS INITIAL.

        APPEND ls_sucesso TO lt_enviar_sucesso.

      ENDIF.

    ENDLOOP.

    me->o_processador_dados->get_o_boletim_sucesso( )->enviar_boletins( i_t_sucesso = lt_enviar_sucesso ).

    " Boletim enviado com sucesso
    MESSAGE TEXT-t04 TYPE 'S'.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
* Classe..: LCL_RELATORIO_ERRO
*---------------------------------------------------------------------*
* Descrição:
*  Classe de relatório para exibir boletins que apresentaram erros.
*
* Responsabilidades:
*  - Exibir ALV com boletins de erro
*  - Permitir tentativa de reenvio ao sistema GEO
*
* Dependências:
*  - LCL_RELATORIO, CL_SALV_TABLE, LCL_POPUP_CONFIRMACAO
*
* Notas:
*  - Usa o status-pf ZSTATUS_BOLETIM
*---------------------------------------------------------------------*
CLASS lcl_relatorio_erro DEFINITION
                         INHERITING FROM lcl_relatorio.

  PUBLIC SECTION.

    METHODS:
      constructor IMPORTING i_o_processador_dados TYPE REF TO lcl_processador_dados
                            i_t_erro              TYPE lif_tipos_global=>ty_t_return_error.

  PROTECTED SECTION.
    METHODS:
      enviar_boletins_para_geo REDEFINITION.

  PRIVATE SECTION.
    DATA:
          t_erro TYPE lif_tipos_global=>ty_t_return_error.

ENDCLASS.

CLASS lcl_relatorio_erro IMPLEMENTATION.

  METHOD constructor.

    super->constructor( i_o_processador_dados ).
    me->t_erro   = i_t_erro.

    TRY.

        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = me->o_alv
          CHANGING
            t_table      = me->t_erro ).

        me->configurar_alv( ).
        me->adicionar_botao( ).
        me->adicionar_evento( ).

      CATCH cx_root.
        " Ocorreu um erro ao gerar o ALV...
    ENDTRY.

  ENDMETHOD.

  METHOD enviar_boletins_para_geo.

    DATA: lt_rows        TYPE salv_t_row,
          lv_index       TYPE sytabix,
          lt_enviar_erro TYPE lif_tipos_global=>ty_t_return_error.

    lt_rows = me->o_alv->get_selections( )->get_selected_rows( ).

    IF lt_rows IS INITIAL.
      " Selecione pelo menos um registro
      MESSAGE TEXT-t03 TYPE 'S'.
      RETURN.
    ENDIF.

    IF lcl_popup_confirmacao=>confirmar_envio_boletim( ) IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_rows INTO lv_index.

      READ TABLE me->t_erro INTO DATA(ls_erro) INDEX lv_index.
      IF sy-subrc IS INITIAL.

        APPEND ls_erro TO lt_enviar_erro.

      ENDIF.

    ENDLOOP.

    me->o_processador_dados->get_o_boletim_erro( )->enviar_boletins( i_t_erro = lt_enviar_erro ).

    " Boletim enviado com sucesso
    MESSAGE TEXT-t04 TYPE 'S'.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
* Classe..: LCL_RELATORIO_PRINCIPAL
*---------------------------------------------------------------------*
* Descrição:
*  Classe principal de exibição de dados do relatório de boletins.
*
* Responsabilidades:
*  - Exibir ALV consolidado com todos os boletins processados
*  - Tratar eventos de clique, seleção e botões
*  - Permitir detalhamento por tipo de envio (sucesso/erro)
*  - Realizar envio em lote para o sistema GEO
*
* Dependências:
*  - LCL_RELATORIO, LCL_RELATORIO_SUCESSO, LCL_RELATORIO_ERRO,
*    CL_SALV_TABLE, LCL_PROCESSADOR_DADOS
*
* Notas:
*  - Usa o status-pf ZSTATUS_BOLETIM
*---------------------------------------------------------------------*
CLASS lcl_relatorio_principal DEFINITION
                              INHERITING FROM lcl_relatorio.

  PUBLIC SECTION.

    METHODS:
      constructor IMPORTING i_o_processador_dados TYPE REF TO lcl_processador_dados.

  PROTECTED SECTION.
    METHODS:
      adicionar_evento         REDEFINITION,
      enviar_boletins_para_geo REDEFINITION.

  PRIVATE SECTION.

    DATA:
          t_saida TYPE lif_tipos_global=>ty_t_saida.

    METHODS:
      exibir_dados_envio FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column,

      exibir_detalhes_sucesso IMPORTING i_s_saida TYPE zpps0016,
      exibir_detalhes_erro    IMPORTING i_s_saida TYPE zpps0016.

ENDCLASS.

CLASS lcl_relatorio_principal IMPLEMENTATION.

  METHOD constructor.

    super->constructor( i_o_processador_dados ).
    me->v_pf_status = 'ZSTATUS_BOLETIM'.
    t_saida = me->o_processador_dados->t_saida.

    TRY.

        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = me->o_alv
          CHANGING
            t_table      = me->t_saida ).

        me->configurar_alv( ).
        me->adicionar_botao( ).
        me->adicionar_evento( ).

      CATCH cx_root.
        " Ocorreu um erro ao gerar o ALV...

    ENDTRY.

  ENDMETHOD.

  METHOD adicionar_evento.

    super->adicionar_evento( ).

    DATA(lo_events) = me->o_alv->get_event( ).
    DATA(lo_event_table) = CAST cl_salv_events_table( lo_events ).

    SET HANDLER me->exibir_dados_envio FOR lo_event_table.

  ENDMETHOD.

  METHOD exibir_dados_envio.

    DATA: ls_saida TYPE zpps0016.

    READ TABLE me->t_saida INTO ls_saida INDEX row.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF column = lcl_botoes=>botao-envio.

      IF ls_saida-envio = lif_constantes_global=>c_icon_sucesso.

        me->exibir_detalhes_sucesso( ls_saida ).

      ELSE.

        me->exibir_detalhes_erro( ls_saida ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD exibir_detalhes_sucesso.

    DATA:
          lo_relatorio_sucesso TYPE REF TO lcl_relatorio_sucesso.

    TRY.

        DATA(lt_sucesso) = me->o_processador_dados->get_o_boletim_sucesso( )->get_items_boletim( i_s_saida ).

        CREATE OBJECT lo_relatorio_sucesso
          EXPORTING
            i_o_processador_dados = me->o_processador_dados
            i_t_sucesso           = lt_sucesso.

        lo_relatorio_sucesso->exibir( ).

      CATCH cx_root.
        " Ocorreu algum erro...
    ENDTRY.

  ENDMETHOD.

  METHOD exibir_detalhes_erro.

    DATA:
          lo_relatorio_erro TYPE REF TO lcl_relatorio_erro.

    TRY.

        DATA(lt_erro) = me->o_processador_dados->get_o_boletim_erro( )->get_items_boletim( i_s_saida ).

        CREATE OBJECT lo_relatorio_erro
          EXPORTING
            i_o_processador_dados = me->o_processador_dados
            i_t_erro              = lt_erro.

        lo_relatorio_erro->exibir( ).


      CATCH cx_root.
        " Ocorreu algum erro...
    ENDTRY.

  ENDMETHOD.

  METHOD enviar_boletins_para_geo.

    DATA: lt_rows  TYPE salv_t_row,
          lv_index TYPE sytabix.

    lt_rows = me->o_alv->get_selections( )->get_selected_rows( ).

    IF lt_rows IS INITIAL.
      " Selecione pelo menos um boletim
      MESSAGE TEXT-t01 TYPE 'S'.
      RETURN.
    ENDIF.

    IF lcl_popup_confirmacao=>confirmar_envio_boletim( ) IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_rows INTO lv_index.

      READ TABLE me->t_saida INTO DATA(ls_saida) INDEX lv_index.
      IF sy-subrc IS INITIAL.

        IF ls_saida-envio = lif_constantes_global=>c_icon_sucesso.

          DATA(lt_sucesso) = me->o_processador_dados->get_o_boletim_sucesso( )->get_items_boletim( ls_saida ).

          IF lt_sucesso IS NOT INITIAL.

            me->o_processador_dados->get_o_boletim_sucesso( )->enviar_boletins( i_t_sucesso = lt_sucesso ).

          ENDIF.

        ELSEIF ls_saida-envio = lif_constantes_global=>c_icon_erro.

          DATA(lt_erro) = me->o_processador_dados->get_o_boletim_erro( )->get_items_boletim( ls_saida ).

          IF lt_erro IS NOT INITIAL.

            me->o_processador_dados->get_o_boletim_erro( )->enviar_boletins( i_t_erro = lt_erro ).

          ENDIF.

        ENDIF.

      ENDIF.

    ENDLOOP.


    " Boletins enviados com sucesso
    MESSAGE TEXT-t02 TYPE 'S'.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
* Classe..: LCL_PROGRAMA
*---------------------------------------------------------------------*
* Descrição:
*  Classe principal que coordena a execução do programa.
*
* Responsabilidades:
*  - Instanciar e orquestrar as classes de dados, processamento e exibição
*  - Iniciar todo o fluxo desde a leitura até o relatório
*
* Dependências:
*  - LCL_GERENCIADOR_DADOS, LCL_PROCESSADOR_DADOS, LCL_RELATORIO
*
* Notas:
*  - Executada a partir do report ZPPR029
*---------------------------------------------------------------------*
CLASS lcl_programa DEFINITION.

  PUBLIC SECTION.

    CLASS-DATA:
      w_tela TYPE  lif_tipos_global=>ty_tela.

    METHODS:
      constructor IMPORTING i_r_nrobol TYPE lif_tipos_global=>ty_r_nrobol
                            i_r_werks  TYPE lif_tipos_global=>ty_r_werks
                            i_r_dtmvto TYPE lif_tipos_global=>ty_r_dtmvto,
      iniciar.

    DATA:
          o_relatorio         TYPE REF TO lcl_relatorio_principal.

  PRIVATE SECTION.

    DATA:
      o_gerenciador_dados TYPE REF TO lcl_gerenciador_dados,
      o_processador_dados TYPE REF TO lcl_processador_dados.

ENDCLASS.

CLASS lcl_programa IMPLEMENTATION.

  METHOD constructor.

    CREATE OBJECT o_gerenciador_dados
      EXPORTING
        i_r_nrobol = i_r_nrobol
        i_r_werks  = i_r_werks
        i_r_dtmvto = i_r_dtmvto.

  ENDMETHOD.

  METHOD iniciar.

    me->o_gerenciador_dados->iniciar( ).

    CREATE OBJECT me->o_processador_dados
      EXPORTING
        i_o_gerenciador_dados = me->o_gerenciador_dados.

    me->o_processador_dados->iniciar( ).

    CREATE OBJECT me->o_relatorio
      EXPORTING
        i_o_processador_dados = me->o_processador_dados.

    me->o_relatorio->exibir( ).

  ENDMETHOD.

ENDCLASS.
