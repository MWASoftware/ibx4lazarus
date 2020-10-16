
Alter Table EMPLOYEE
  Add Photo Blob;

Create View DeptList
As

with recursive Depts As (
Select DEPT_NO, DEPARTMENT, HEAD_DEPT, cast(DEPARTMENT  as VarChar(256)) as DEPT_PATH,
cast(DEPT_NO as VarChar(64)) as DEPT_KEY_PATH
From DEPARTMENT Where HEAD_DEPT is NULL 
UNION ALL
Select D.DEPT_NO, D.DEPARTMENT, D.HEAD_DEPT, Depts.DEPT_PATH ||  ' / ' || D.DEPARTMENT as DEPT_PATH,
Depts.DEPT_KEY_PATH || ';' || D.DEPT_NO as DEPT_KEY_PATH
From DEPARTMENT  D
JOIN Depts On D.HEAD_DEPT = Depts.DEPT_NO
)

Select A.EMP_NO, A.FIRST_NAME, A.LAST_NAME, A.PHONE_EXT, A.HIRE_DATE, A.DEPT_NO, A.JOB_CODE, 
A.JOB_GRADE, A.JOB_COUNTRY, A.SALARY, A.FULL_NAME, D.DEPT_PATH, D.DEPT_KEY_PATH, A.PHOTO
From EMPLOYEE A
JOIN Depts D On D.DEPT_NO = A.DEPT_NO;

Update EMPLOYEE Set Photo =:MUGSHOT Where Emp_no = 2;

Update DBVERSIONINFO Set VersionNo = 2;
Commit;
