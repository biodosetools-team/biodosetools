The following methods for error calculation can be selected:

- Merkle's method: considering both curve calibration and measurement errors.

By default, a 83% confidence interval (CI) is used for both curve calibration and measurement errors. This results in a total CI of 95%. 
In case no variance-covariance matrix is provided, the measurement errors CI will be adjusted accordingly to have a resulting total CI of 95%.

- Dolphin (only for partial-body estimation). 

This approach is based on the delta method, and has a CI of 95% as well.

