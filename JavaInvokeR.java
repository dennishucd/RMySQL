import java.io.IOException;

public class JavaInvokeR {

	public static void main(String[] args) {
		System.out.println("args.length = "+args.length);
		//parameters validation: wsguid, csecguid, pcode, predict_days(optinal, default_value=7)
		if (args.length < 2) {
			System.out.println("The number of parameters should no less than three.");
			
			return ;
		}
		
		String rPath = "/opt/regression.R";
		String os = System.getProperty("os.name").toLowerCase();

		int ret = 0;
		try {
			// ubuntu
			if (os.indexOf("linux") != -1) {
				StringBuffer cmd = new StringBuffer();
				cmd.append("Rscript ");
				cmd.append(rPath);
				cmd.append(" ");
				cmd.append(args[0]);
				cmd.append(" ");
				cmd.append(args[1]);
				cmd.append(" ");
				cmd.append(args[2]);
				
				if (args.length>3) {
					cmd.append(" ");
					cmd.append(args[3]);
				}
				
				System.out.println("cmd = " + cmd);
				
				ret = Runtime.getRuntime().exec(cmd.toString()).waitFor();

			} else { // windows
//				cmd = "R CMD Rscript " + rPath;
//				Runtime.getRuntime().exec(cmd);
			}
		} catch (IOException e) {
			e.printStackTrace();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		
		if (ret == 0) {
			//success
			System.out.println("success");
		} else {
			System.out.println("failure");
		}
	}
}