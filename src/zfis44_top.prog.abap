*&---------------------------------------------------------------------*
*& Include          ZFIS44_TOP
*&---------------------------------------------------------------------*

  TYPES: BEGIN OF ty_proc_email,
           chave_nfe   TYPE zib_nfe_dist_ter-chave_nfe,
           forne_cnpj  TYPE zib_nfe_dist_ter-forne_cnpj,
           forne_razao TYPE zib_nfe_dist_ter-forne_razao,
           dt_emissao  TYPE zib_nfe_dist_ter-dt_emissao,
           vl_fatura   TYPE zib_nfe_dist_ter-vl_total_fatura,
           dias_recusa TYPE string,
         END OF ty_proc_email,

         BEGIN OF ty_proc_email_fiscal,
           empresa     TYPE edobrincoming-company_code,
           cnpj_cpf    TYPE edobrincoming-supplier_cnpj_cpf,
           nfenum      TYPE edobrincoming-nfenum,
           series      TYPE edobrincoming-series,
           issue_date  TYPE edobrincoming-issue_date,
           plant       TYPE edobrincoming-plant,
           total_value TYPE edobrincoming-total_value,
           accesskey   TYPE edobrincoming-accesskey,
         END OF ty_proc_email_fiscal.


  TYPES: tb_proc_email TYPE TABLE OF ty_proc_email,
         tb_proc_email_fiscal TYPE TABLE OF ty_proc_email_fiscal.
