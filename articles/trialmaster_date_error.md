# Table Reading Date Error

When building a table in **TrialBuilder**, an error may occur if:

- The variables have been created.
- The export is **not rebuilt** but only **refreshed**.

In this situation, the **Text** format may incorrectly be applied to
date fields. This makes the whole table unreadable in R, throwing the
following error:

> CRF error: some date-type columns are stored as text

------------------------------------------------------------------------

## Steps to Correct the Issue

1.  Change the table attribute from `Automatic` to **Custom**.  
    ![List of tables - VS switched to Custom](img/tm_date_error_1.png)

2.  Identify the **date-type variables** that are currently in:

    - `char / 50. / date9.`  
      Open VS to locate the two variables with incorrect formats.  
      ![Identify variables with incorrect
      format](img/tm_date_error_2.png)

3.  Change these variables to the format:

    - `num / 8. / date9.`  
      ![Corrected formats](img/tm_date_error_3.png)
