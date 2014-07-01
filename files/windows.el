;; fixing various windows-related bullshit

(when (eq system-type 'windows-nt)
  (setq-default exec-path '("d:/progs/perl/perl/site/bin/"
                            "d:/progs/perl/perl/bin/"
                            "d:/progs/gnuutils/bin/"
                            "c:/Program Files (x86)/NVIDIA Corporation/PhysX/Common"
                            "C:/Windows/system32"
                            "C:/Windows"
                            "C:/Windows/System32/Wbem"
                            "C:/Windows/System32/WindowsPowerShell/v1.0/"
                            "d:/progs/MiKTeX/miktex/bin/"
                            "D:/progs/Git/cmd"
                            "D:/progs/TortoiseHg/"
                            "d:/progs/Aspell/bin/"
                            "C:/Users/Matus/AppData/Roaming/MiKTeX/2.9/miktex/bin/"
                            "d:/progs/emacs-24.1/bin"))

  (eval-after-load 'inf-ruby
    '(setq-default
      inf-ruby-implementations
      '(("ruby"     . "d:/progs/Ruby200/bin/irb --prompt default -r irb/completion")
        ("jruby"    . "jruby -S irb --prompt default -r irb/completion")
        ("rubinius" . "rbx -r irb/completion")
        ("yarv"     . "irb1.9 -r irb/completion")
        ("macruby"  . "macirb -r irb/completion")
        ("pry"      . "pry"))))

  (defadvice shell-command (around fix-encoding activate)
    (let ((coding-system-for-read 'cp1250))
      ad-do-it)))
