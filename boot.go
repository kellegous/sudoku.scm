package main

import (
	"flag"
	"html/template"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
)

var templates = map[string]string{
	"nginx.conf": `
worker_processes  1;
daemon off;

events {
  worker_connections 1024;
}

http {
  access_log {{.AccessLog}};
  error_log {{.ErrorLog}};

  server {
    listen {{.Port}};
    root {{.Root}};

    location ~ \.php$ {
      fastcgi_param REQUEST_METHOD $request_method;
      fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;
      fastcgi_pass 127.0.0.1:9000;
      include fastcgi_params;
    }

  }
}
  `,

	"fastcgi_params": `
fastcgi_param  QUERY_STRING       $query_string;
fastcgi_param  REQUEST_METHOD     $request_method;
fastcgi_param  CONTENT_TYPE       $content_type;
fastcgi_param  CONTENT_LENGTH     $content_length;

fastcgi_param  SCRIPT_NAME        $fastcgi_script_name;
fastcgi_param  REQUEST_URI        $request_uri;
fastcgi_param  DOCUMENT_URI       $document_uri;
fastcgi_param  DOCUMENT_ROOT      $document_root;
fastcgi_param  SERVER_PROTOCOL    $server_protocol;
fastcgi_param  HTTPS              $https if_not_empty;

fastcgi_param  GATEWAY_INTERFACE  CGI/1.1;
fastcgi_param  SERVER_SOFTWARE    nginx/$nginx_version;

fastcgi_param  REMOTE_ADDR        $remote_addr;
fastcgi_param  REMOTE_PORT        $remote_port;
fastcgi_param  SERVER_ADDR        $server_addr;
fastcgi_param  SERVER_PORT        $server_port;
fastcgi_param  SERVER_NAME        $server_name;

# PHP only, required if PHP was built with --enable-force-cgi-redirect
fastcgi_param  REDIRECT_STATUS    200;
  `,
}

// Data ...
type Data struct {
	Port      int
	Root      string
	AccessLog string
	ErrorLog  string
}

func render(dst string, tpl *template.Template, data *Data) error {
	w, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer w.Close()

	return tpl.Execute(w, data)
}

func writeConfig(dst string, port int, root string) error {
	data := Data{
		Port:      port,
		Root:      filepath.Join(root, "pub"),
		AccessLog: filepath.Join(root, "access.log"),
		ErrorLog:  filepath.Join(root, "error.log"),
	}

	for path, content := range templates {
		t, err := template.New(path).Parse(content)
		if err != nil {
			return err
		}

		if err := render(filepath.Join(dst, path), t, &data); err != nil {
			return err
		}
	}
	return nil
}

func run(c *exec.Cmd) (*os.Process, error) {
	c.Stdout = os.Stdout
	c.Stderr = os.Stderr
	if err := c.Start(); err != nil {
		return nil, err
	}
	return c.Process, nil
}

func deleteOnSignal(dir string) {
	ch := make(chan os.Signal)
	signal.Notify(ch, os.Interrupt, os.Kill)
	go func() {
		<-ch
		os.RemoveAll(dir)
	}()
}

func main() {
	flagPort := flag.Int("port", 8080, "")
	flagRoot := flag.String("root", ".",
		"")
	flag.Parse()

	tmp, err := ioutil.TempDir("", "")
	if err != nil {
		log.Panic(err)
	}
	deleteOnSignal(tmp)

	root, err := filepath.Abs(*flagRoot)
	if err != nil {
		log.Panic(err)
	}

	if err := writeConfig(tmp, *flagPort, root); err != nil {
		log.Panic(err)
	}

	if _, err := run(exec.Command(filepath.Join(root, "server"))); err != nil {
		log.Panic(err)
	}

	p, err := run(exec.Command("nginx", "-c", filepath.Join(tmp, "nginx.conf")))
	if err != nil {
		log.Panic(err)
	}

	if _, err := p.Wait(); err != nil {
		log.Panic(err)
	}
}
